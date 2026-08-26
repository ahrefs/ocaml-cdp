(* The production transport: a libcurl WebSocket driven through Curl_lwt,
   receive frames in the write callback, reassemble fragmented messages.
   Use Curl.ws_send for the outgoing direction. *)

let default_max_message_size = 256 * 1024 * 1024

(** One incoming message passed [max_message_size]; carries the cap in bytes. In-flight calls fail with this and the
    connection closes. *)
exception Message_too_large of int

let () =
  Printexc.register_printer (function
    | Message_too_large cap -> Some (Printf.sprintf "Cdp_lwt.Curl_transport.Message_too_large: cap %d bytes" cap)
    | _other_exception -> None)

let configure ~url =
  let handle = Curl.init () in
  Curl.set_url handle url;
  Curl.set_connecttimeout handle 30;
  (* a CDP connection is long-lived: no overall transfer timeout *)
  Curl.set_timeout handle 0;
  handle

(* sending can start only after the WebSocket handshake; retry briefly
   instead of tracking handshake state.
   [alive] is re-checked on every attempt — the transfer can end during the
   sleep, and touching the handle after Curl.cleanup segfaults *)
let rec send_with_retry ~alive ~death_error handle payload ~attempts_left =
  match !alive with
  | false -> Lwt.fail (death_error ())
  | true ->
  match Curl.ws_send handle payload [ Curl.CURLWS_TEXT ] with
  | _bytes_sent -> Lwt.return_unit
  | exception failure ->
  match attempts_left > 0 with
  | false -> Lwt.fail failure
  | true ->
    let%lwt () = Lwt_unix.sleep 0.1 in
    send_with_retry ~alive ~death_error handle payload ~attempts_left:(attempts_left - 1)

(** [connect ~url ()] opens a WebSocket and returns the transport for {!Connection.create}.

    - [url]: a [ws://] DevTools address, e.g. {!Chrome.launch}'s [ws_url].
    - [max_message_size]: cap for one incoming message *)
let connect ~url ?(max_message_size = default_max_message_size) () : Transport.t Lwt.t =
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
        if is_payload then begin
          Buffer.add_string message_buffer chunk;
          let is_final = (not (List.mem Curl.CURLWS_CONT frame.Curl.flags)) && frame.Curl.bytesleft = 0 in
          if is_final then begin
            push_incoming (Some (Buffer.contents message_buffer));
            Buffer.clear message_buffer
          end
        end);
      String.length chunk);
  (* run the transfer in the background; when it ends — server closed, network
     died, or we aborted — the incoming stream ends with None *)
  Lwt.async (fun () ->
    let%lwt (_finished : Curl.curlCode) =
      try%lwt Curl_lwt.perform handle with
      | Curl.CurlException (code, _errno, _message) -> Lwt.return code
      | _unexpected -> Lwt.return Curl.CURLE_RECV_ERROR
    in
    (* dead before cleanup: send/close must never touch a freed handle *)
    alive := false;
    push_incoming None;
    Curl.cleanup handle;
    Lwt.return_unit);
  let transport =
    {
      Transport.send = (fun payload -> send_with_retry ~alive ~death_error handle payload ~attempts_left:50);
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
          (* tell the server we are leaving; if the connection is already
               dead this fails, which is fine — perform is ending anyway *)
          try ignore (Curl.ws_send handle "" [ Curl.CURLWS_CLOSE ] : int) with _already_dead -> ());
          Lwt.return_unit);
    }
  in
  Lwt.return transport
