(* The connection: owns a transport, numbers outgoing commands, remembers
   which parser belongs to which id, and routes incoming messages — answers
   to their waiting callers, events to their subscribers. *)

(** Chrome answered the command with an error. *)
exception Protocol_error of Cdp.Envelope.error

(** The transport closed; in-flight calls and event waits fail with this. A transport that died with its own error (like
    [Curl_transport.Message_too_large]) delivers that error instead. *)
exception Connection_closed

(** [call ~timeout] gave up waiting; carries the command name. *)
exception Call_timeout of string

(* human-readable exception messages: without a registered printer these
   show as "Protocol_error(_)", hiding what Chrome actually said *)
let () =
  Printexc.register_printer (function
    | Call_timeout command_name -> Some ("Cdp_lwt.Connection.Call_timeout: " ^ command_name)
    | Protocol_error { code; message; data = _extra } ->
      Some (Printf.sprintf "Cdp_lwt.Connection.Protocol_error: %d %s" code message)
    | _other_exception -> None)

(* what can happen to a command that was sent *)
type outcome =
  | Result of Cdp_json.t
  | Protocol_failure of Cdp.Envelope.error
  | Died of exn

type event_waiter = {
  wanted_session : string option;
  deliver : Cdp_json.t -> unit;
  abandon : exn -> unit;
  (* persistent waiters (on_event) survive delivery; one-shot (next_event) do not *)
  persistent : bool;
}

type t = {
  transport : Transport.t;
  next_id : int ref;
  (* id -> what to do with the outcome of that command *)
  pending : (int, outcome -> unit) Hashtbl.t;
  (* event name -> everyone waiting for its next occurrence *)
  event_waiters : (string, event_waiter) Hashtbl.t;
  closed : unit Lwt.t;
  set_closed : unit Lwt.u;
}

let session_string session = Option.map Cdp.Target.Session_id.to_string session

let fail_everything connection ~failure =
  let calls = Hashtbl.fold (fun _id resolve accumulated -> resolve :: accumulated) connection.pending [] in
  let waiters = Hashtbl.fold (fun _name waiter accumulated -> waiter :: accumulated) connection.event_waiters [] in
  Hashtbl.reset connection.pending;
  Hashtbl.reset connection.event_waiters;
  List.iter (fun resolve -> resolve (Died failure)) calls;
  List.iter (fun waiter -> waiter.abandon failure) waiters

let handle_response connection ~id ~outcome =
  match Hashtbl.find_opt connection.pending id with
  | None -> () (* a response nobody waits for anymore, e.g. after a timeout *)
  | Some resolve ->
    Hashtbl.remove connection.pending id;
    resolve outcome

let remove_waiter connection ~name waiter =
  let remaining =
    Hashtbl.find_all connection.event_waiters name |> List.filter (fun candidate -> not (candidate == waiter))
  in
  while Hashtbl.mem connection.event_waiters name do
    Hashtbl.remove connection.event_waiters name
  done;
  List.iter (fun kept -> Hashtbl.add connection.event_waiters name kept) (List.rev remaining)

let handle_event connection ~name ~params ~session =
  let matches waiter =
    match waiter.wanted_session with
    | None -> true
    | Some wanted -> Some wanted = session
  in
  let all_waiters = Hashtbl.find_all connection.event_waiters name in
  match List.exists matches all_waiters with
  | false -> ()
  | true ->
    (* deliver to every match; one-shot waiters are then removed, persistent
       ones stay. find_all returns newest first; re-adding the reverse
       preserves order. *)
    let delivered = List.filter matches all_waiters in
    let remaining = List.filter (fun waiter -> (not (matches waiter)) || waiter.persistent) all_waiters in
    while Hashtbl.mem connection.event_waiters name do
      Hashtbl.remove connection.event_waiters name
    done;
    List.iter (fun waiter -> Hashtbl.add connection.event_waiters name waiter) (List.rev remaining);
    List.iter (fun waiter -> waiter.deliver params) delivered

let stop connection ~failure =
  fail_everything connection ~failure;
  Lwt.wakeup_later connection.set_closed ();
  Lwt.return_unit

let rec read_loop connection =
  match%lwt connection.transport.Transport.receive () with
  | exception transport_failure -> stop connection ~failure:transport_failure
  | None -> stop connection ~failure:Connection_closed
  | Some raw ->
    let parse_wire text =
      match Yojson.Basic.from_string text with
      | json -> Some json
      | exception Yojson.Json_error _parse_error -> None
    in
    let parsed =
      match parse_wire raw with
      | Some _ as json -> json
      | None ->
        (* Chrome can emit unpaired \uXXXX surrogates that strict parsers
           reject; repair them and retry before dropping the message *)
        parse_wire (Cdp.Json.repair_lone_surrogates raw)
    in
    (match parsed with
    | None -> () (* not JSON: ignore, keep the connection alive *)
    | Some json ->
    match Cdp.Envelope.parse json with
    | Error _classification_error -> () (* unknown shape: ignore *)
    | Ok (Response { id; outcome = Ok result_json; session = _ignored }) ->
      handle_response connection ~id ~outcome:(Result result_json)
    | Ok (Response { id; outcome = Error protocol_error; session = _ignored }) ->
      handle_response connection ~id ~outcome:(Protocol_failure protocol_error)
    | Ok (Event { name; params; session }) -> handle_event connection ~name ~params ~session);
    read_loop connection

(** [create transport] starts the read loop on [transport] and returns a connection ready for {!call}. *)
let create transport =
  let closed, set_closed = Lwt.wait () in
  let connection =
    { transport; next_id = ref 0; pending = Hashtbl.create 32; event_waiters = Hashtbl.create 16; closed; set_closed }
  in
  Lwt.async (fun () -> read_loop connection);
  connection

(** A promise that resolves once the connection is closed — by {!close}, by the peer, or by a transport failure. *)
let closed connection = connection.closed

(** Close the transport. Every in-flight call and event wait fails with {!Connection_closed}, and {!closed} resolves. *)
let close connection =
  (* the transport signals the read loop with a final None, which fails the
     in-flight calls and event waits and resolves [closed] *)
  connection.transport.Transport.close ()

let with_timeout ~timeout ~command_name waiting =
  match timeout with
  | None -> waiting
  | Some seconds ->
    let expired =
      let%lwt () = Lwt_unix.sleep seconds in
      Lwt.fail (Call_timeout command_name)
    in
    Lwt.pick [ waiting; expired ]

let is_closed connection =
  match Lwt.state connection.closed with
  | Lwt.Return () -> true
  | Lwt.Sleep -> false
  | Lwt.Fail _never_fails -> false

(** [call connection command] sends [command] and waits for its typed result.
    - [session]: target one attached session.
    - [timeout]: seconds to wait, forever when absent. On expiry raises {!Call_timeout} and drops the late response.
    Failures: {!Protocol_error} — Chrome answered with an error; {!Connection_closed} — the connection closed cleanly;
    the transport's own error (like [Curl_transport.Transport_failure]) — it died of one. Cancelling the returned
    promise forgets the command. *)
let call connection ?session ?timeout (command : 'result Cdp.Command.t) : 'result Lwt.t =
  if is_closed connection then Lwt.fail Connection_closed
  else begin
    incr connection.next_id;
    let id = !(connection.next_id) in
    (* Lwt.task, not Lwt.wait: callers may Lwt.cancel the returned promise *)
    let result_promise, resolve_result = Lwt.task () in
    Lwt.on_cancel result_promise (fun () -> Hashtbl.remove connection.pending id);
    Hashtbl.replace connection.pending id (fun outcome ->
      match outcome with
      | Died failure -> Lwt.wakeup_later_exn resolve_result failure
      | Protocol_failure protocol_error -> Lwt.wakeup_later_exn resolve_result (Protocol_error protocol_error)
      | Result result_json ->
      match command.Cdp.Command.parse result_json with
      | parsed -> Lwt.wakeup_later resolve_result parsed
      | exception parse_failure -> Lwt.wakeup_later_exn resolve_result parse_failure);
    let request =
      Cdp.Envelope.request ~id ?session:(session_string session) ~name:command.Cdp.Command.name
        ~params:command.Cdp.Command.params ()
    in
    let%lwt () = connection.transport.Transport.send (Yojson.Basic.to_string request) in
    (* on timeout (or any failure), forget the pending entry so a very late
     response is dropped instead of waking a dead promise *)
    Lwt.catch
      (fun () -> with_timeout ~timeout ~command_name:command.Cdp.Command.name result_promise)
      (fun failure ->
        Hashtbl.remove connection.pending id;
        Lwt.fail failure)
  end

(** [next_event connection event] waits for one occurrence of [event] arriving {e after} this call — subscribe first,
    then trigger. [session] filters to one session. One-shot; for every occurrence use {!on_event}. *)
let next_event connection ?session (event : 'params Cdp.Event.t) : 'params Lwt.t =
  if is_closed connection then Lwt.fail Connection_closed
  else begin
    let params_promise, resolve_params = Lwt.task () in
    let waiter =
      {
        wanted_session = session_string session;
        deliver =
          (fun params ->
            match event.Cdp.Event.parse params with
            | parsed -> Lwt.wakeup_later resolve_params parsed
            | exception parse_failure -> Lwt.wakeup_later_exn resolve_params parse_failure);
        abandon = (fun failure -> Lwt.wakeup_later_exn resolve_params failure);
        persistent = false;
      }
    in
    Hashtbl.add connection.event_waiters event.Cdp.Event.name waiter;
    Lwt.on_cancel params_promise (fun () -> remove_waiter connection ~name:event.Cdp.Event.name waiter);
    params_promise
  end

(** A persistent subscription: [handler] runs on every matching event until the returned unsubscribe function is called
    or the connection closes. Payloads that fail to parse are skipped. *)
let on_event connection ?session (event : 'params Cdp.Event.t) (handler : 'params -> unit) : unit -> unit =
  let waiter =
    {
      wanted_session = session_string session;
      deliver =
        (fun params ->
          match event.Cdp.Event.parse params with
          | parsed -> handler parsed
          | exception _unparseable_payload -> ());
      abandon = (fun _connection_closed -> ());
      persistent = true;
    }
  in
  Hashtbl.add connection.event_waiters event.Cdp.Event.name waiter;
  fun () -> remove_waiter connection ~name:event.Cdp.Event.name waiter
