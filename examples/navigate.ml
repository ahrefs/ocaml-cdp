(* The whole story in one program: launch a headless Chrome, open a page,
   navigate, wait for the load event, and read the title back. *)

let page_url = "data:text/html,<title>Hello from OCaml CDP</title><h1>It works</h1>"

let () =
  Lwt_main.run
    begin
      let%lwt chrome = Cdp_lwt.Chrome.launch () in
      Printf.printf "chrome is listening on %s\n%!" chrome.ws_url;
      let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:chrome.ws_url () in
      let connection = Cdp_lwt.Connection.create transport in
      let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:10.0 command in
      (* a fresh page target, attached as a flat session *)
      let%lwt created =
        call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt attached =
        call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:created.target_id ~flatten:true ()))
      in
      let session = attached.session_id in
      (* enable page events, subscribe to the load event FIRST, then navigate *)
      let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
      let loaded = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
      let%lwt _navigation =
        call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url:page_url ()))
      in
      let%lwt _fired = loaded in
      (* ask the page for its title through Runtime.evaluate *)
      let%lwt evaluated =
        call ~session (Cdp.Runtime.Evaluate.command (Cdp.Runtime.Evaluate.make_params ~expression:"document.title" ()))
      in
      (match evaluated.result.value with
      | None -> print_endline "page title: <no value>"
      | Some title -> Printf.printf "page title: %s\n" (Cdp.Json.show title));
      let%lwt () = Cdp_lwt.Connection.close connection in
      chrome.kill ()
    end
