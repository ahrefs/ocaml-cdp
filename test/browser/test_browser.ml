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
      let%lwt () = capped_chrome.kill () in
      Lwt.return_unit
    end

let () = print_endline "all browser smoke tests passed"
