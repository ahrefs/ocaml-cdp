(* Smoke tests against a real headless Chrome — the only place the curl
   transport is exercised. Five checks chosen by command SHAPE, so every
   kind of exchange the protocol has is proven on a live browser. *)

let pass name = Printf.printf "PASS %s\n" name
let page_url = "data:text/html,<title>smoke</title><p>hello</p>"

let () =
  Lwt_main.run
    begin
      let%lwt chrome = Cdp_lwt.Chrome.launch () in
      let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:chrome.ws_url () in
      let connection = Cdp_lwt.Connection.create transport in
      let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:10.0 command in
      (* shape 1: no params, typed result *)
      let%lwt version = call Cdp.Browser.Get_version.command in
      assert (String.length version.product > 0);
      pass ("no-params command with typed result (" ^ version.product ^ ")");
      (* a session for the page-scoped shapes *)
      let%lwt created =
        call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt attached =
        call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:created.target_id ~flatten:true ()))
      in
      let session = attached.session_id in
      (* shape 2: zero-return command through a session *)
      let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
      pass "zero-return command through a session";
      (* shape 3: event subscription — subscribe first, then trigger *)
      let loaded = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
      let%lwt _navigation =
        call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url:page_url ()))
      in
      let%lwt fired = loaded in
      assert (Cdp.Network.Monotonic_time.to_float fired.timestamp > 0.0);
      pass "navigate and wait for the load event";
      (* shape 4: params + typed result *)
      let%lwt evaluated =
        call ~session (Cdp.Runtime.Evaluate.command (Cdp.Runtime.Evaluate.make_params ~expression:"document.title" ()))
      in
      (match evaluated.result.value with
      | Some (`String "smoke") -> ()
      | _unexpected -> assert false);
      pass "params + typed result (Runtime.evaluate)";
      (* a command far larger than one socket write: libcurl accepts big
         frames in pieces, and a dropped piece would hang this call forever *)
      let big_length = (3 * 1024 * 1024) + 12345 in
      let big_expression = Printf.sprintf "%S.length" (String.make big_length 'a') in
      let%lwt measured =
        call ~session (Cdp.Runtime.Evaluate.command (Cdp.Runtime.Evaluate.make_params ~expression:big_expression ()))
      in
      (match measured.result.value with
      | Some (`Int reported_length) -> assert (reported_length = big_length)
      | _unexpected -> assert false);
      pass "a multi-megabyte command payload round-trips uncorrupted";
      (* the mirror direction: a multi-megabyte RESPONSE arrives in dozens of
         socket chunks and must reassemble intact — and the default message
         cap must admit it *)
      let incoming_length = (3 * 1024 * 1024) + 54321 in
      let%lwt returned =
        call ~session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params ~expression:(Printf.sprintf "'x'.repeat(%d)" incoming_length) ()))
      in
      (match returned.result.value with
      | Some (`String text) ->
        assert (String.length text = incoming_length);
        assert (String.for_all (fun ch -> ch = 'x') text)
      | _unexpected -> assert false);
      pass "a multi-megabyte response reassembles intact under the default cap";
      (* shape 5: state roundtrip — write a cookie, read it back typed *)
      let%lwt cookie_set =
        call ~session
          (Cdp.Network.Set_cookie.command
             (Cdp.Network.Set_cookie.make_params ~name:"cdp_smoke" ~value:"42" ~url:"https://example.com/" ()))
      in
      assert cookie_set.success;
      let%lwt cookies =
        call ~session
          (Cdp.Network.Get_cookies.command (Cdp.Network.Get_cookies.make_params ~urls:[ "https://example.com/" ] ()))
      in
      let smoke_cookie = List.find (fun (cookie : Cdp.Network.cookie) -> cookie.name = "cdp_smoke") cookies.cookies in
      assert (smoke_cookie.value = "42");
      pass "state roundtrip: cookie written and read back typed";
      let%lwt () = Cdp_lwt.Connection.close connection in
      let%lwt () = chrome.kill () in
      (* shape 6: the message size cap — a response over the cap must fail
         the call with the typed error and close the connection. Own Chrome:
         a browser endpoint accepts one client, and reusing the endpoint of
         the connection closed above could be refused mid-teardown, which
         would end as a plain close and not exercise the cap. *)
      let%lwt capped_chrome = Cdp_lwt.Chrome.launch () in
      let%lwt capped_transport = Cdp_lwt.Curl_transport.connect ~url:capped_chrome.ws_url ~max_message_size:64 () in
      let capped_connection = Cdp_lwt.Connection.create capped_transport in
      let%lwt () =
        try%lwt
          let%lwt (_version : Cdp.Browser.Get_version.result) =
            Cdp_lwt.Connection.call capped_connection ~timeout:10.0 Cdp.Browser.Get_version.command
          in
          assert false
        with Cdp_lwt.Curl_transport.Message_too_large 64 -> Lwt.return_unit
      in
      pass "a message over max_message_size fails typed and closes the connection";
      (* closing after the transport died and freed its handle must be a
         safe no-op *)
      let%lwt () = Cdp_lwt.Connection.close capped_connection in
      pass "close after transport death is a safe no-op";
      let%lwt () = capped_chrome.kill () in
      (* shape 7: a binary that exits without announcing must fail with a
         clear message and clean up its profile directory *)
      (* count only this library's naming scheme (8 hex chars): the shared
         temp dir also holds old PID-named profiles from other processes *)
      let profile_dirs () =
        let prefix = "cdp-chrome-" in
        let is_hex ch = (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') in
        Sys.readdir (Filename.get_temp_dir_name ())
        |> Array.to_list
        |> List.filter (fun entry ->
          String.starts_with ~prefix entry
          && String.length entry = String.length prefix + 8
          && String.for_all is_hex (String.sub entry (String.length prefix) 8))
        |> List.length
      in
      let dirs_before = profile_dirs () in
      let%lwt () =
        try%lwt
          let%lwt (_chrome : Cdp_lwt.Chrome.t) = Cdp_lwt.Chrome.launch ~executable:"/bin/false" () in
          assert false
        with Cdp_lwt.Chrome.Launch_failed (Cdp_lwt.Chrome.Exited_early { stderr = [] }) -> Lwt.return_unit
      in
      assert (profile_dirs () = dirs_before);
      pass "a silently exiting binary fails typed and leaves no profile";
      (* a binary that is not there at all: refused before anything is spawned *)
      let%lwt () =
        try%lwt
          let%lwt (_chrome : Cdp_lwt.Chrome.t) = Cdp_lwt.Chrome.launch ~executable:"not-a-chrome-anywhere" () in
          assert false
        with Cdp_lwt.Chrome.Launch_failed (Cdp_lwt.Chrome.Executable_not_found "not-a-chrome-anywhere") ->
          Lwt.return_unit
      in
      assert (profile_dirs () = dirs_before);
      pass "a missing executable fails typed before spawning";
      (* the other launch failure: a binary that stays alive but never
         announces — the timeout must fire, kill the process, and clean up *)
      let quiet_binary = Filename.concat (Filename.get_temp_dir_name ()) "cdp-test-quiet-binary" in
      let script = open_out quiet_binary in
      output_string script "#!/bin/sh\nexec sleep 30\n";
      close_out script;
      Unix.chmod quiet_binary 0o755;
      let%lwt () =
        try%lwt
          let%lwt (_chrome : Cdp_lwt.Chrome.t) = Cdp_lwt.Chrome.launch ~executable:quiet_binary ~timeout:1.0 () in
          assert false
        with Cdp_lwt.Chrome.Launch_failed (Cdp_lwt.Chrome.Announce_timeout { timeout = waited; stderr = [] }) ->
          assert (waited = 1.0);
          Lwt.return_unit
      in
      assert (profile_dirs () = dirs_before);
      Sys.remove quiet_binary;
      pass "a binary that never announces times out typed and cleans up";
      (* with_launch: the bracket must kill and clean up also when the
         callback raises *)
      let%lwt () =
        try%lwt
          Cdp_lwt.Chrome.with_launch (fun bracketed ->
            assert (String.length bracketed.Cdp_lwt.Chrome.ws_url > 0);
            Lwt.fail Exit)
        with Exit -> Lwt.return_unit
      in
      assert (profile_dirs () = dirs_before);
      pass "with_launch kills chrome and removes the profile when the callback raises";
      (* shape 8: a page that logs ~600KB to Chrome's stderr during ONE call.
         A pipe holds ~64KB; without the drain loops Chrome blocks on its own
         logging mid-call, the response never arrives, and this times out *)
      let%lwt noisy_chrome = Cdp_lwt.Chrome.launch ~extra_args:[ "--enable-logging=stderr" ] () in
      let%lwt noisy_transport = Cdp_lwt.Curl_transport.connect ~url:noisy_chrome.ws_url () in
      let noisy_connection = Cdp_lwt.Connection.create noisy_transport in
      let noisy_call ?session command = Cdp_lwt.Connection.call noisy_connection ?session ~timeout:20.0 command in
      let%lwt noisy_created =
        noisy_call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt noisy_attached =
        noisy_call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:noisy_created.target_id ~flatten:true ()))
      in
      let noisy_session = noisy_attached.session_id in
      let%lwt logged =
        noisy_call ~session:noisy_session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params
                ~expression:"for (let i = 0; i < 4000; i++) console.log('drain'.repeat(30)); 'still alive'"
                ()))
      in
      (match logged.result.value with
      | Some (`String "still alive") -> ()
      | _unexpected -> assert false);
      pass "chrome survives flooding its own stderr mid-call (pipes are drained)";
      (* shape 9: half an emoji — JavaScript can split a two-unit character,
         and Chrome sends the lone half as an unpaired \uXXXX escape. The
         message must be repaired to the replacement character, not dropped
         (a drop would hang this call until its timeout) *)
      let%lwt half_emoji =
        noisy_call ~session:noisy_session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params ~expression:"'\240\159\152\128'.substring(0, 1)" ()))
      in
      (match half_emoji.result.value with
      | Some (`String "\239\191\189") -> ()
      | _unexpected -> assert false);
      pass "half an emoji from page content is repaired, not dropped";
      (* the OTHER half: a lone low surrogate parses without error, so it
         must be repaired before parsing or invalid UTF-8 reaches the caller *)
      let%lwt low_half =
        noisy_call ~session:noisy_session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params ~expression:"'\240\159\152\128'.substring(1, 2)" ()))
      in
      (match low_half.result.value with
      | Some (`String repaired_value) ->
        assert (String.is_valid_utf_8 repaired_value);
        assert (repaired_value = "\239\191\189")
      | _unexpected -> assert false);
      pass "the low half of an emoji arrives repaired, never as invalid utf-8";
      (* a whole-number JavaScript value past OCaml's 63 bits: Chrome prints
         it as a bare integer, which must arrive as a float, not kill the
         message and hang this call *)
      let%lwt huge_number =
        noisy_call ~session:noisy_session
          (Cdp.Runtime.Evaluate.command (Cdp.Runtime.Evaluate.make_params ~expression:"2**62" ()))
      in
      (match huge_number.result.value with
      | Some (`Float value) -> assert (value = 2. ** 62.)
      | _unexpected -> assert false);
      pass "an integer past 63 bits arrives as a float, not a dropped message";
      (* two big concurrent sends: the send mutex must keep frames whole *)
      let big_eval label =
        noisy_call ~session:noisy_session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params
                ~expression:(Printf.sprintf "%S.length" (String.make (2 * 1024 * 1024) label))
                ()))
      in
      let%lwt first_big, second_big = Lwt.both (big_eval 'a') (big_eval 'b') in
      (match first_big.result.value, second_big.result.value with
      | Some (`Int first_length), Some (`Int second_length) ->
        assert (first_length = 2 * 1024 * 1024);
        assert (second_length = 2 * 1024 * 1024)
      | _unexpected -> assert false);
      pass "two concurrent multi-megabyte sends stay uncorrupted";
      (* a target that dies mid-wait: Chrome announces it with
         Target.detachedFromTarget, and the session's waiters must fail
         typed instead of hanging *)
      let%lwt doomed_created =
        noisy_call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt doomed_attached =
        noisy_call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:doomed_created.target_id ~flatten:true ()))
      in
      let doomed_wait =
        Cdp_lwt.Connection.next_event noisy_connection ~session:doomed_attached.session_id
          Cdp.Page.Load_event_fired.event
      in
      let%lwt (_closed : Cdp.Target.Close_target.result) =
        noisy_call
          (Cdp.Target.Close_target.command (Cdp.Target.Close_target.make_params ~target_id:doomed_created.target_id))
      in
      let%lwt () =
        Lwt.pick
          [
            (match%lwt Lwt.map ignore doomed_wait with
            | exception Cdp_lwt.Connection.Session_detached _gone -> Lwt.return_unit
            | exception _other -> assert false
            | () -> assert false);
            (let%lwt () = Lwt_unix.sleep 10.0 in
             Lwt.fail (Failure "session detach did not fail the waiter"));
          ]
      in
      pass "a closed target fails its session's waiters typed";
      let%lwt () = Cdp_lwt.Connection.close noisy_connection in
      let%lwt () = noisy_chrome.kill () in
      (* shape 10: close with zero traffic — an idle peer never volunteers a
         frame, so close itself must end the transfer and resolve [closed] *)
      let%lwt idle_chrome = Cdp_lwt.Chrome.launch () in
      let%lwt idle_transport = Cdp_lwt.Curl_transport.connect ~url:idle_chrome.ws_url () in
      let idle_connection = Cdp_lwt.Connection.create idle_transport in
      let%lwt () = Cdp_lwt.Connection.close idle_connection in
      let%lwt () =
        Lwt.pick
          [
            Cdp_lwt.Connection.closed idle_connection;
            (let%lwt () = Lwt_unix.sleep 5.0 in
             Lwt.fail (Failure "close on an idle connection did not resolve closed"));
          ]
      in
      pass "close on an idle connection resolves closed and ends the transfer";
      (* a send racing close() must fail fast and typed, not touch the
         closing transfer: close set the flag synchronously above *)
      let%lwt () =
        try%lwt
          let%lwt () = idle_transport.Cdp_lwt.Transport.send "too late" in
          assert false
        with Cdp_lwt.Transport.Closed -> Lwt.return_unit
      in
      pass "send after close fails fast with Transport.Closed";
      (* the fixed-port path: ask the OS for a free port, launch on it, and
         the announced address must carry exactly that port *)
      let probe = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.bind probe (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      let chosen_port =
        match Unix.getsockname probe with
        | Unix.ADDR_INET (_loopback, port) -> port
        | Unix.ADDR_UNIX _impossible -> assert false
      in
      Unix.close probe;
      let%lwt () =
        Cdp_lwt.Chrome.with_launch ~port:chosen_port (fun fixed ->
          let expected_prefix = Printf.sprintf "ws://127.0.0.1:%d/" chosen_port in
          assert (String.starts_with ~prefix:expected_prefix fixed.Cdp_lwt.Chrome.ws_url);
          Lwt.return_unit)
      in
      pass "a fixed devtools port is honored in the announced address";
      (* a server that accepts TCP but never answers the handshake: connect
         must fail typed within its deadline, not hang forever *)
      let silent = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Unix.bind silent (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen silent 8;
      let silent_port =
        match Unix.getsockname silent with
        | Unix.ADDR_INET (_loopback, port) -> port
        | Unix.ADDR_UNIX _impossible -> assert false
      in
      let%lwt () =
        try%lwt
          let%lwt (_transport : Cdp_lwt.Transport.t) =
            Cdp_lwt.Curl_transport.connect
              ~url:(Printf.sprintf "ws://127.0.0.1:%d/" silent_port)
              ~connect_timeout:1.0 ()
          in
          assert false
        with Cdp_lwt.Curl_transport.Transport_failure { message = "no websocket handshake within 1s"; _ } ->
          Lwt.return_unit
      in
      Unix.close silent;
      pass "a silent tcp server fails connect typed within the deadline";
      (* shape 11: nothing listens on port 1 — connect must fail typed, not
         "succeed" and die later as a clean close *)
      let%lwt () =
        try%lwt
          let%lwt (_transport : Cdp_lwt.Transport.t) = Cdp_lwt.Curl_transport.connect ~url:"ws://127.0.0.1:1/" () in
          assert false
        with Cdp_lwt.Curl_transport.Transport_failure _refused -> Lwt.return_unit
      in
      pass "connect to a dead address fails typed";
      (* shape 12: an HTTP endpoint that answers 200 instead of accepting the
         upgrade — Chrome's own /json/version, reached on the DevTools port *)
      let authority_end =
        match String.index_from_opt idle_chrome.ws_url (String.length "ws://") '/' with
        | Some slash -> slash
        | None -> String.length idle_chrome.ws_url
      in
      let version_url = String.sub idle_chrome.ws_url 0 authority_end ^ "/json/version" in
      let%lwt () =
        try%lwt
          let%lwt (_transport : Cdp_lwt.Transport.t) = Cdp_lwt.Curl_transport.connect ~url:version_url () in
          assert false
        with Cdp_lwt.Curl_transport.Transport_failure _not_an_upgrade -> Lwt.return_unit
      in
      pass "a refused websocket upgrade fails connect typed";
      let%lwt () = idle_chrome.kill () in
      Lwt.return_unit
    end

let () = print_endline "all browser smoke tests passed"
