(* Connection logic tests over an in-memory fake transport: no browser, no
   network. The fake records what was sent and lets the test inject incoming
   messages or close the pipe. *)

let pass name = Printf.printf "PASS %s\n" name

type fake = {
  transport : Cdp_lwt.Transport.t;
  sent : string list ref;
  inject : string -> unit;
  kill : unit -> unit;
}

exception Fake_transport_failure

let make_fake () =
  let incoming, push_incoming = Lwt_stream.create () in
  let sent = ref [] in
  let transport =
    {
      Cdp_lwt.Transport.send =
        (fun payload ->
          sent := payload :: !sent;
          Lwt.return_unit);
      receive = (fun () -> Lwt_stream.get incoming);
      close =
        (fun () ->
          push_incoming None;
          Lwt.return_unit);
    }
  in
  { transport; sent; inject = (fun raw -> push_incoming (Some raw)); kill = (fun () -> push_incoming None) }

(* let the connection's read loop run over the injected messages *)
let settle () = Lwt.pause ()

let get_version = Cdp.Browser.Get_version.command
let enable_security = Cdp.Security.Enable.command

let () =
  Lwt_main.run
    begin
      (* 1. out-of-order responses: two calls, answers arrive reversed *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let first = Cdp_lwt.Connection.call connection get_version in
    let second = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"id":2,"result":{}}|};
    fake.inject
      {|{"id":1,"result":{"protocolVersion":"1.3","product":"Chrome/1","revision":"r","userAgent":"u","jsVersion":"14"}}|};
    let%lwt version = first in
    let%lwt () = second in
    assert (version.product = "Chrome/1");
    pass "out-of-order responses reach the right callers";

    (* 2. an event interleaved between request and response *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let waiting_event = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    let waiting_call = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":1.5}}|};
    fake.inject {|{"id":1,"result":{}}|};
    let%lwt fired = waiting_event in
    let%lwt () = waiting_call in
    assert (Cdp.Network.Monotonic_time.to_float fired.timestamp = 1.5);
    pass "event interleaved between request and response";

    (* 3. connection death with two calls and an event wait in flight *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let first = Cdp_lwt.Connection.call connection get_version in
    let second = Cdp_lwt.Connection.call connection enable_security in
    let waiting_event = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    fake.kill ();
    let all_rejected promise =
      match%lwt promise with
      | exception Cdp_lwt.Connection.Connection_closed -> Lwt.return_true
      | exception _other -> Lwt.return_false
      | _result -> Lwt.return_false
    in
    let%lwt first_rejected = all_rejected (Lwt.map ignore first) in
    let%lwt second_rejected = all_rejected second in
    let%lwt event_rejected = all_rejected (Lwt.map ignore waiting_event) in
    assert (first_rejected && second_rejected && event_rejected);
    let%lwt () = Cdp_lwt.Connection.closed connection in
    pass "connection death rejects everything in flight, nothing hangs";

    (* 4. a protocol error becomes a typed exception *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let failing = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"id":1,"error":{"code":-32601,"message":"Security.enable was not found"}}|};
    let%lwt () =
      match%lwt failing with
      | exception Cdp_lwt.Connection.Protocol_error { code = -32601; _ } -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    pass "protocol error becomes Protocol_error with the code";

    (* 5. events are routed by session *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let session_a = Cdp.Target.Session_id.of_string "A" in
    let for_a = Cdp_lwt.Connection.next_event connection ~session:session_a Cdp.Page.Load_event_fired.event in
    let for_anyone = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":2.5},"sessionId":"B"}|};
    let%lwt () = settle () in
    assert (Lwt.state for_a = Lwt.Sleep);
    let%lwt anyone = for_anyone in
    assert (Cdp.Network.Monotonic_time.to_float anyone.timestamp = 2.5);
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":3.5},"sessionId":"A"}|};
    let%lwt fired_a = for_a in
    assert (Cdp.Network.Monotonic_time.to_float fired_a.timestamp = 3.5);
    pass "events are routed by session";

    (* 6. what went on the wire is a correct envelope *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let (_ignored : unit Lwt.t) =
      Cdp_lwt.Connection.call connection ~session:(Cdp.Target.Session_id.of_string "S1") enable_security
    in
    let%lwt () = settle () in
    (match !(fake.sent) with
    | [ raw ] ->
      assert (
        Yojson.Basic.from_string raw
        = `Assoc [ "id", `Int 1; "method", `String "Security.enable"; "sessionId", `String "S1" ])
    | _unexpected -> assert false);
    pass "sent envelope carries id, method, and session";

    (* 7. timeout fires, and the late response is dropped harmlessly *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let slow = Cdp_lwt.Connection.call connection ~timeout:0.05 get_version in
    let%lwt () =
      match%lwt Lwt.map ignore slow with
      | exception Cdp_lwt.Connection.Call_timeout "Browser.getVersion" -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    fake.inject
      {|{"id":1,"result":{"protocolVersion":"1.3","product":"late","revision":"r","userAgent":"u","jsVersion":"14"}}|};
    let followup = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"id":2,"result":{}}|};
    let%lwt () = followup in
    pass "timeout raises Call_timeout; the late response is dropped";

    (* 8. a response that fails to parse rejects that call only *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let poisoned = Cdp_lwt.Connection.call connection get_version in
    fake.inject {|{"id":1,"result":{"unexpected":"shape"}}|};
    let%lwt () =
      match%lwt Lwt.map ignore poisoned with
      | exception Cdp_lwt.Connection.Protocol_error _classified -> assert false
      | exception _parse_failure -> Lwt.return_unit
      | () -> assert false
    in
    let survivor = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"id":2,"result":{}}|};
    let%lwt () = survivor in
    pass "unparseable result rejects one call, the connection survives";

    (* 9. wire garbage is ignored and the connection keeps working *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let resilient = Cdp_lwt.Connection.call connection enable_security in
    fake.inject "this is not even json";
    fake.inject {|{"neither":"response","nor":"event"}|};
    fake.inject {|{"id":1,"result":{}}|};
    let%lwt () = resilient in
    pass "garbage on the wire is ignored, the connection keeps working";

    (* 10. calling on a closed connection fails fast instead of hanging *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    fake.kill ();
    let%lwt () = Cdp_lwt.Connection.closed connection in
    let%lwt () =
      match%lwt Cdp_lwt.Connection.call connection enable_security with
      | exception Cdp_lwt.Connection.Connection_closed -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    let%lwt () =
      match%lwt Lwt.map ignore (Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event) with
      | exception Cdp_lwt.Connection.Connection_closed -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    pass "call and next_event on a closed connection fail fast";

    (* 11. two waiters on one event both fire; another event is undisturbed *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let first_waiter = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    let second_waiter = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    let other_event = Cdp_lwt.Connection.next_event connection Cdp.Page.Frame_detached.event in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":4.5}}|};
    let%lwt first_fired = first_waiter in
    let%lwt second_fired = second_waiter in
    assert (Cdp.Network.Monotonic_time.to_float first_fired.timestamp = 4.5);
    assert (Cdp.Network.Monotonic_time.to_float second_fired.timestamp = 4.5);
    let%lwt () = settle () in
    assert (Lwt.state (Lwt.map ignore other_event) = Lwt.Sleep);
    pass "concurrent waiters both fire; other events are undisturbed";

    (* 12. re-armed one-shot waiters see events in order *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let first_round = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":1.0}}|};
    let%lwt first_seen = first_round in
    let second_round = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":2.0}}|};
    let%lwt second_seen = second_round in
    assert (Cdp.Network.Monotonic_time.to_float first_seen.timestamp = 1.0);
    assert (Cdp.Network.Monotonic_time.to_float second_seen.timestamp = 2.0);
    pass "re-armed waiters see events in order";

    (* 13. local close (not remote death) rejects in-flight work and resolves closed *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let in_flight = Cdp_lwt.Connection.call connection enable_security in
    let%lwt () = Cdp_lwt.Connection.close connection in
    let%lwt () =
      match%lwt in_flight with
      | exception Cdp_lwt.Connection.Connection_closed -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    let%lwt () = Cdp_lwt.Connection.closed connection in
    pass "local close rejects in-flight calls and resolves closed";

    (* 14. Lwt.cancel on a call cleans up; the connection keeps working *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let cancelled = Cdp_lwt.Connection.call connection get_version in
    let%lwt () = settle () in
    Lwt.cancel cancelled;
    fake.inject
      {|{"id":1,"result":{"protocolVersion":"1.3","product":"late","revision":"r","userAgent":"u","jsVersion":"14"}}|};
    let survivor = Cdp_lwt.Connection.call connection enable_security in
    fake.inject {|{"id":2,"result":{}}|};
    let%lwt () = survivor in
    assert (Lwt.state (Lwt.map ignore cancelled) = Lwt.Fail Lwt.Canceled);
    pass "a cancelled call cleans up and the connection keeps working";

    (* 15. persistent on_event sees every occurrence until unsubscribed;
           one-shot waiters alongside it are unaffected *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let seen = ref [] in
    let unsubscribe =
      Cdp_lwt.Connection.on_event connection Cdp.Page.Load_event_fired.event (fun fired ->
        seen := Cdp.Network.Monotonic_time.to_float fired.timestamp :: !seen)
    in
    let one_shot = Cdp_lwt.Connection.next_event connection Cdp.Page.Load_event_fired.event in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":1.0}}|};
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":2.0}}|};
    let%lwt first_shot = one_shot in
    assert (Cdp.Network.Monotonic_time.to_float first_shot.timestamp = 1.0);
    let%lwt () = settle () in
    assert (!seen = [ 2.0; 1.0 ]);
    unsubscribe ();
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":3.0}}|};
    let%lwt () = settle () in
    assert (!seen = [ 2.0; 1.0 ]);
    pass "on_event sees every occurrence until unsubscribed";

    (* 16. a transport that dies with its own error delivers it to in-flight
           work, instead of the generic Connection_closed *)
    let incoming, push_incoming = Lwt_stream.create () in
    let dying_transport =
      {
        Cdp_lwt.Transport.send = (fun _payload -> Lwt.return_unit);
        receive =
          (fun () ->
            match%lwt Lwt_stream.get incoming with
            | Some _ as message -> Lwt.return message
            | None -> Lwt.fail Fake_transport_failure);
        close =
          (fun () ->
            push_incoming None;
            Lwt.return_unit);
      }
    in
    let connection = Cdp_lwt.Connection.create dying_transport in
    let in_flight = Cdp_lwt.Connection.call connection enable_security in
    push_incoming None;
    let%lwt () =
      match%lwt in_flight with
      | exception Fake_transport_failure -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    let%lwt () = Cdp_lwt.Connection.closed connection in
    pass "a transport's own error reaches in-flight calls typed";

    (* 17. a message the json parser rejects only for an unpaired surrogate
       escape is repaired and delivered, not silently dropped *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let awaiting = Cdp_lwt.Connection.call connection enable_security in
    fake.inject "{\"id\":1,\"result\":{},\"note\":\"\\ud83d\"}";
    let%lwt () = awaiting in
    pass "a lone-surrogate message is repaired, not dropped";

    (* 18. a message holding an integer past OCaml's 63 bits is reparsed
       tolerantly and delivered, not silently dropped *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let awaiting = Cdp_lwt.Connection.call connection enable_security in
    fake.inject "{\"id\":1,\"result\":{},\"big\":4611686018427387904}";
    let%lwt () = awaiting in
    pass "an oversized-integer message is delivered, not dropped";

    (* 19. a detached session fails ITS in-flight work typed; work on other
       sessions (and the root) survives untouched *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let session = Cdp.Target.Session_id.of_string "dead-session" in
    let session_call = Cdp_lwt.Connection.call connection ~session enable_security in
    let session_event = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
    let root_call = Cdp_lwt.Connection.call connection get_version in
    fake.inject {|{"method":"Target.detachedFromTarget","params":{"sessionId":"dead-session","targetId":"t"}}|};
    let%lwt () =
      match%lwt session_call with
      | exception Cdp_lwt.Connection.Session_detached detached ->
        assert (Cdp.Target.Session_id.to_string detached = "dead-session");
        Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    let%lwt () =
      match%lwt Lwt.map ignore session_event with
      | exception Cdp_lwt.Connection.Session_detached _detached -> Lwt.return_unit
      | exception _other -> assert false
      | () -> assert false
    in
    fake.inject
      {|{"id":2,"result":{"protocolVersion":"1.3","product":"alive","revision":"r","userAgent":"u","jsVersion":"14"}}|};
    let%lwt version = root_call in
    assert (version.product = "alive");
    pass "a detached session fails its calls and waiters; the rest survives";

    (* 20. on_event ~session sees only its session's events, and only until
       unsubscribed; an unfiltered subscription alongside sees everything *)
    let fake = make_fake () in
    let connection = Cdp_lwt.Connection.create fake.transport in
    let session_a = Cdp.Target.Session_id.of_string "session-a" in
    let seen_a = ref [] in
    let seen_all = ref [] in
    let unsubscribe_a =
      Cdp_lwt.Connection.on_event connection ~session:session_a Cdp.Page.Load_event_fired.event (fun fired ->
        seen_a := Cdp.Network.Monotonic_time.to_float fired.timestamp :: !seen_a)
    in
    let (_unsubscribe_all : unit -> unit) =
      Cdp_lwt.Connection.on_event connection Cdp.Page.Load_event_fired.event (fun fired ->
        seen_all := Cdp.Network.Monotonic_time.to_float fired.timestamp :: !seen_all)
    in
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":1.0},"sessionId":"session-a"}|};
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":2.0},"sessionId":"session-b"}|};
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":3.0}}|};
    let%lwt () = settle () in
    assert (!seen_a = [ 1.0 ]);
    assert (!seen_all = [ 3.0; 2.0; 1.0 ]);
    unsubscribe_a ();
    fake.inject {|{"method":"Page.loadEventFired","params":{"timestamp":4.0},"sessionId":"session-a"}|};
    let%lwt () = settle () in
    assert (!seen_a = [ 1.0 ]);
    assert (!seen_all = [ 4.0; 3.0; 2.0; 1.0 ]);
    pass "on_event with a session filters to that session until unsubscribed";
    Lwt.return_unit
    end

let () = print_endline "all connection tests passed"
