(* Open a fresh visible Chrome on [url] and print its page, network, and
   console events as they happen — browse in the window, watch the terminal.

   The optional [cookie] (name=value) is set for [url] before navigating, so
   a session cookie copied from your normal browser logs this Chrome in.

     dune exec examples/tail.exe -- http://localhost:3333 session=abc123 *)

let usage = "usage: tail [url] [cookie as name=value]"
let default_url = "https://example.com"

let parse_cookie raw =
  match String.index_opt raw '=' with
  | Some equals_position ->
    String.sub raw 0 equals_position, String.sub raw (equals_position + 1) (String.length raw - equals_position - 1)
  | None ->
    prerr_endline ("cookie must look like name=value, got: " ^ raw);
    exit 1

let console_argument_text (argument : Cdp.Runtime.remote_object) =
  match argument.value with
  | Some value -> Yojson.Basic.to_string value
  | None -> Option.value argument.description ~default:"<object>"

let () =
  let arguments = Array.to_list Sys.argv in
  let url =
    match List.nth_opt arguments 1 with
    | Some url -> url
    | None ->
      prerr_endline usage;
      default_url
  in
  let cookie = Option.map parse_cookie (List.nth_opt arguments 2) in
  Lwt_main.run
    begin
      let%lwt chrome = Cdp_lwt.Chrome.launch ~headless:false () in
      let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:chrome.ws_url () in
      let connection = Cdp_lwt.Connection.create transport in
      let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:20.0 command in
      let%lwt targets = call (Cdp.Target.Get_targets.command (Cdp.Target.Get_targets.make_params ())) in
      let page =
        match List.find_opt (fun (target : Cdp.Target.target_info) -> target.type_ = "page") targets.target_infos with
        | Some page -> page
        | None -> failwith "the launched Chrome has no tab"
      in
      let%lwt attached =
        call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:page.target_id ~flatten:true ()))
      in
      let session = attached.session_id in
      let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
      let%lwt () = call ~session (Cdp.Network.Enable.command (Cdp.Network.Enable.make_params ())) in
      let%lwt () = call ~session Cdp.Runtime.Enable.command in
      let (_stop_requests : unit -> unit) =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Network.Request_will_be_sent.event
          (fun (event : Cdp.Network.Request_will_be_sent.params) ->
          Printf.printf "-> %s %s\n%!" event.request.method_ event.request.url)
      in
      let (_stop_responses : unit -> unit) =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Network.Response_received.event
          (fun (event : Cdp.Network.Response_received.params) ->
          Printf.printf "<- %d %s\n%!" event.response.status event.response.url)
      in
      let (_stop_navigations : unit -> unit) =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Page.Frame_navigated.event
          (fun (event : Cdp.Page.Frame_navigated.params) ->
          match event.frame.parent_id with
          | Some _child_frame -> ()
          | None -> Printf.printf "page: %s\n%!" event.frame.url)
      in
      let (_stop_loads : unit -> unit) =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Page.Load_event_fired.event
          (fun (_event : Cdp.Page.Load_event_fired.params) -> Printf.printf "page: loaded\n%!")
      in
      let (_stop_console : unit -> unit) =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Runtime.Console_api_called.event
          (fun (event : Cdp.Runtime.Console_api_called.params) ->
          Printf.printf "console: %s\n%!" (String.concat " " (List.map console_argument_text event.args)))
      in
      let%lwt () =
        match cookie with
        | None -> Lwt.return_unit
        | Some (cookie_name, cookie_value) ->
          let%lwt set =
            call ~session
              (Cdp.Network.Set_cookie.command
                 (Cdp.Network.Set_cookie.make_params ~name:cookie_name ~value:cookie_value ~url ()))
          in
          (match set.success with
          | true -> Printf.printf "cookie %s set for %s\n%!" cookie_name url
          | false -> Printf.printf "cookie %s was NOT accepted for %s\n%!" cookie_name url);
          Lwt.return_unit
      in
      Printf.printf "tailing %s — browse in the Chrome window, Ctrl+C to stop\n%!" url;
      let%lwt _navigation = call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url ())) in
      (* wait forever: the promise below never resolves, so the connection
         stays open and the handlers above keep printing *)
      fst (Lwt.wait ())
    end
