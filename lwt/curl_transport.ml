(* The production transport: a libcurl WebSocket driven through Curl_lwt,
   receive frames in the write callback, reassemble fragmented messages.
   Use Curl.ws_send for the outgoing direction. *)

let default_max_message_size = 256 * 1024 * 1024
let default_max_retained_buffer = 50 * 1024 * 1024

(** One incoming message passed [max_message_size]; carries the cap in bytes. In-flight calls fail with this and the
    connection closes. *)
exception Message_too_large of int

(** WebSocket transfer failed: unreachable server, refused upgrade, or a mid-session network error. [connect] fails with
    this when the handshake cannot complete; in-flight calls fail with it when an established connection dies. *)
exception
  Transport_failure of {
    code : Curl.curlCode;
    message : string;
  }

let () =
  Printexc.register_printer (function
    | Message_too_large cap -> Some (Printf.sprintf "Cdp_lwt.Curl_transport.Message_too_large: cap %d bytes" cap)
    | Transport_failure { code = _; message } -> Some ("Cdp_lwt.Curl_transport.Transport_failure: " ^ message)
    | _other_exception -> None)

let configure ~url =
  let handle = Curl.init () in
  Curl.set_url handle url;
  Curl.set_connecttimeout handle 30;
  (* a CDP connection is long-lived: no overall transfer timeout *)
  Curl.set_timeout handle 0;
  handle

(* 50 x 0.1s: how long one send may sit with no byte accepted before giving
   up — covers both the pre-handshake window and a full socket buffer *)
let default_stall_budget = 50

(* libcurl may accept only PART of a large frame per ws_send call ("short
   send"); the remainder must be re-offered from the returned offset or the
   frame is silently truncated on the wire.
   
   stalls (pre-handshake, full socket) are retried briefly;
   the budget resets whenever bytes are accepted.
   [alive] is re-checked on every attempt — the transfer can end
   during a sleep, and touching the handle after Curl.cleanup segfaults.
   
   send that gives up mid-frame has corrupted the stream, so it kills the
   whole transport via [abort].
   [ws_send] is injected so tests can drive short sends and stalls deterministically. *)
let send_all ?(stall_budget = default_stall_budget) ~alive ~death_error ~abort ~ws_send payload =
  let total = String.length payload in
  let rec from_offset ~offset ~stalls_left =
    match !alive with
    | false -> Lwt.fail (death_error ())
    | true ->
    match ws_send (String.sub payload offset (total - offset)) with
    | sent when offset + sent >= total -> Lwt.return_unit
    | sent when sent > 0 -> from_offset ~offset:(offset + sent) ~stalls_left:stall_budget
    | _no_progress -> stalled ~offset ~stalls_left (Failure "cdp-lwt: websocket send made no progress")
    | exception failure -> stalled ~offset ~stalls_left failure
  and stalled ~offset ~stalls_left failure =
    match stalls_left with
    | 0 ->
      if offset > 0 then abort failure;
      Lwt.fail failure
    | _tries_remaining ->
      let%lwt () = Lwt_unix.sleep 0.1 in
      from_offset ~offset ~stalls_left:(stalls_left - 1)
  in
  from_offset ~offset:0 ~stalls_left:stall_budget

type accumulation =
  | Ignored (* not a payload chunk, e.g. a control frame *)
  | Accumulating
  | Complete of string

(* payload chunks accumulate until the final chunk of a message's final
   frame arrives; [Complete] hands the finished message over and resets the
   buffer for the next one. Split out so frame sequences are testable. *)
let accumulate ~max_retained ~message_buffer ~chunk ~is_payload ~is_final =
  match is_payload with
  | false -> Ignored
  | true ->
    Buffer.add_string message_buffer chunk;
    (match is_final with
    | false -> Accumulating
    | true ->
      let message = Buffer.contents message_buffer in
      (* Buffer.clear keeps the grown storage; after an unusually large
         message, release it instead of holding peak capacity for the
         connection's whole life *)
      (match String.length message > max_retained with
      | true -> Buffer.reset message_buffer
      | false -> Buffer.clear message_buffer);
      Complete message)

(** [connect ~url ()] opens a WebSocket, waits for the server to accept the upgrade, and returns the transport for
    {!Connection.create}. Fails with {!Transport_failure} when the server cannot be reached or refuses the upgrade.

    - [url]: a [ws://] DevTools address, e.g. {!Chrome.launch}'s [ws_url].
    - [connect_timeout]: seconds to wait for the server to accept the upgrade (default 30) — a server that opens the
      socket but never answers must not hang the connect forever.
    - [max_message_size]: cap for one incoming message.
    - [max_retained_buffer]: reassembly memory kept between messages (default 50 MB); after delivering a message larger
      than this, the buffer is released instead of holding peak capacity for the connection's life. *)
let connect ~url ?(connect_timeout = 30.) ?(max_message_size = default_max_message_size)
  ?(max_retained_buffer = default_max_retained_buffer) () : Transport.t Lwt.t =
  let handle = configure ~url in
  let incoming, push_incoming = Lwt_stream.create () in
  let message_buffer = Buffer.create 8192 in
  let closing = ref false in
  let alive = ref true in
  let transport_failure = ref None in
  let death_error () =
    match !transport_failure with
    | Some died -> died
    | None -> Transport.Closed
  in
  let abort failure =
    transport_failure := Some failure;
    closing := true
  in
  (* resolved once, by whichever comes first: the accepted upgrade (Ok) or
     the transfer's death (Error) *)
  let connect_outcome, set_connect_outcome = Lwt.wait () in
  let resolve_connect outcome =
    match Lwt.state connect_outcome with
    | Lwt.Sleep -> Lwt.wakeup_later set_connect_outcome outcome
    | _already_resolved -> ()
  in
  Curl.set_writefunction handle (fun chunk ->
    match !closing with
    | true -> 0 (* wrong length aborts the transfer, ending Curl_lwt.perform *)
    | false ->
    match Buffer.length message_buffer + String.length chunk > max_message_size with
    | true ->
      (* the message passed the cap: abort the transfer, the stream ends *)
      transport_failure := Some (Message_too_large max_message_size);
      closing := true;
      0
    | false ->
      (match Curl.ws_meta handle with
      | None -> () (* not a websocket frame; nothing we can use *)
      | Some frame ->
        let is_payload =
          List.mem Curl.CURLWS_TEXT frame.Curl.flags
          || List.mem Curl.CURLWS_BINARY frame.Curl.flags
          || List.mem Curl.CURLWS_CONT frame.Curl.flags
        in
        let is_final = (not (List.mem Curl.CURLWS_CONT frame.Curl.flags)) && frame.Curl.bytesleft = 0 in
        (match accumulate ~max_retained:max_retained_buffer ~message_buffer ~chunk ~is_payload ~is_final with
        | Complete message -> push_incoming (Some message)
        | Ignored | Accumulating -> ()));
      String.length chunk);
  (* the background transfer; whenever it ends, the stream ends with None.
     The promise is kept: with an idle peer only a cancel can end it. *)
  let transfer = Curl_lwt.perform handle in
  (* libcurl forbids removing a transfer from inside its own callbacks, where
     close/abort can run — defer the cancel by one loop tick *)
  let cancel_transfer () =
    Lwt.async (fun () ->
      let%lwt () = Lwt.pause () in
      Lwt.cancel transfer;
      Lwt.return_unit)
  in
  (* the upgrade response arrives through the header callback: a status line,
     then fields, then an empty line ending the block — only a 101 block
     means the websocket is established *)
  let upgrade_accepted = ref false in
  Curl.set_headerfunction handle (fun header ->
    (match String.trim header with
    | "" ->
      (match !upgrade_accepted with
      | true -> resolve_connect (Ok ())
      | false ->
        abort
          (Transport_failure
             (* CURLE_HTTP_NOT_FOUND is ocurl's name for CURLE_HTTP_RETURNED_ERROR *)
             { code = Curl.CURLE_HTTP_NOT_FOUND; message = "server did not accept the websocket upgrade" });
        cancel_transfer ())
    | line when String.starts_with ~prefix:"HTTP/" line ->
      (match String.split_on_char ' ' line with
      | _http_version :: "101" :: _reason -> upgrade_accepted := true
      | _not_an_upgrade -> ())
    | _header_field -> ());
    String.length header);
  Lwt.async (fun () ->
    let%lwt (finished : Curl.curlCode) =
      try%lwt transfer with
      | Curl.CurlException (code, _errno, _message) -> Lwt.return code
      | _cancelled_or_unexpected -> Lwt.return Curl.CURLE_RECV_ERROR
    in
    (* a transfer that died uninvited carries its reason to receive/send *)
    (match !closing, !transport_failure, finished with
    | true, _, _ | _, Some _, _ | _, _, Curl.CURLE_OK -> ()
    | false, None, failed_code ->
      transport_failure := Some (Transport_failure { code = failed_code; message = Curl.strerror failed_code }));
    (* dead before cleanup: send/close must never touch a freed handle *)
    alive := false;
    push_incoming None;
    Curl.cleanup handle;
    resolve_connect (Error (death_error ()));
    Lwt.return_unit);
  (* one frame at a time: a send that stalls mid-frame yields, and a second
     send interleaving into the half-sent frame would corrupt the stream *)
  let send_lock = Lwt_mutex.create () in
  let transport =
    {
      Transport.send =
        (fun payload ->
          Lwt_mutex.with_lock send_lock (fun () ->
            match !closing with
            | true ->
              (* fail fast on send-after-close instead of racing the teardown *)
              Lwt.fail (death_error ())
            | false ->
              send_all ~alive ~death_error
                ~abort:(fun failure ->
                  abort failure;
                  cancel_transfer ())
                ~ws_send:(fun piece ->
                  try Curl.ws_send handle piece [ Curl.CURLWS_TEXT ]
                  with Curl.CurlException (code, _errno, message) -> raise (Transport_failure { code; message }))
                payload));
      receive =
        (fun () ->
          match%lwt Lwt_stream.get incoming with
          | Some _ as message -> Lwt.return message
          | None ->
          match !transport_failure with
          | Some died -> Lwt.fail died
          | None -> Lwt.return_none);
      close =
        (fun () ->
          closing := true;
          (match !alive with
          | false -> () (* the transfer is over and the handle is freed; nothing left to tell the server *)
          | true ->
            (* tell the server we are leaving (best effort — pre-handshake
               this fails, and a send mid-frame must not be interleaved),
               then end the transfer ourselves: an idle peer would never
               send the frame that lets it end on its own *)
            (match Lwt_mutex.is_locked send_lock with
            | true -> () (* a frame is in flight; the cancel below ends the transfer anyway *)
            | false -> try ignore (Curl.ws_send handle "" [ Curl.CURLWS_CLOSE ] : int) with _already_dead -> ());
            cancel_transfer ());
          Lwt.return_unit);
    }
  in
  let handshake_deadline =
    let%lwt () = Lwt_unix.sleep connect_timeout in
    Lwt.return
      (Error
         (Transport_failure
            {
              code = Curl.CURLE_OPERATION_TIMEOUTED;
              message = Printf.sprintf "no websocket handshake within %gs" connect_timeout;
            }))
  in
  match%lwt Lwt.pick [ connect_outcome; handshake_deadline ] with
  | Ok () -> Lwt.return transport
  | Error failure ->
    (* the transfer may still be running against a silent server: record the
       reason and end it, or the handle would linger forever *)
    abort failure;
    cancel_transfer ();
    Lwt.fail failure
