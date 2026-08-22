(* Attach to a Chrome that is already running — one started with
   --remote-debugging-port, possibly owned by another tool. We open our own
   tab, screenshot it, and close only that tab; the browser keeps running
   for its other clients.

     curl -s http://127.0.0.1:<port>/json/version   (* gives webSocketDebuggerUrl *)
     dune exec examples/attach.exe -- ws://127.0.0.1:<port>/devtools/browser/<id> https://example.com example 1320 1037 *)

let usage = "usage: attach <browser websocket url> [url] [name] [width] [height]"

let with_scheme url =
  match String.length url >= 4 && String.sub url 0 4 = "http" with
  | true -> url
  | false -> "https://" ^ url

let default_url = "https://example.com"
let default_name = "attach_example"
let default_viewport_width = 1320
let default_viewport_height = 1037
let screenshots_directory = "screenshots"

(* octal permissions: the owner can write, everyone can enter and read *)
let directory_permissions = 0o755

let save_png ~path ~base64_data =
  match Base64.decode base64_data with
  | Error (`Msg reason) -> failwith ("could not decode the screenshot: " ^ reason)
  | Ok png_bytes ->
    let out = open_out_bin path in
    output_string out png_bytes;
    close_out out;
    Printf.printf "saved %s (%d bytes)\n" path (String.length png_bytes)

let () =
  let arguments = Array.to_list Sys.argv in
  let read_argument position = List.nth_opt arguments position in
  let ws_url =
    match read_argument 1 with
    | Some ws_url -> ws_url
    | None ->
      prerr_endline usage;
      exit 1
  in
  let url =
    match read_argument 2 with
    | None -> default_url
    | Some requested -> with_scheme requested
  in
  let name =
    match read_argument 3 with
    | Some name -> name
    | None -> default_name
  in
  let viewport_width =
    match read_argument 4 with
    | None -> default_viewport_width
    | Some width -> int_of_string width
  in
  let viewport_height =
    match read_argument 5 with
    | Some height -> int_of_string height
    | None -> default_viewport_height
  in
  (try Unix.mkdir screenshots_directory directory_permissions with Unix.Unix_error (Unix.EEXIST, _mkdir, _path) -> ());
  Lwt_main.run
    begin
      let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:ws_url () in
      let connection = Cdp_lwt.Connection.create transport in
      let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:20.0 command in
      let%lwt created =
        call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt attached =
        call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:created.target_id ~flatten:true ()))
      in
      let session = attached.session_id in
      let%lwt () =
        call ~session
          (Cdp.Emulation.Set_device_metrics_override.command
             (Cdp.Emulation.Set_device_metrics_override.make_params ~width:viewport_width ~height:viewport_height
                ~device_scale_factor:1.0 ~mobile:false ()))
      in
      let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
      let loaded = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
      let%lwt _navigation = call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url ())) in
      let%lwt _fired = loaded in
      (* let late JavaScript settle, like a rendering vendor would *)
      let%lwt () = Lwt_unix.sleep 1.5 in
      let%lwt shot = call ~session (Cdp.Page.Capture_screenshot.command (Cdp.Page.Capture_screenshot.make_params ())) in
      save_png
        ~path:(Filename.concat screenshots_directory (Printf.sprintf "%s-attached.png" name))
        ~base64_data:shot.data;
      (* close only our tab: the browser belongs to someone else *)
      let%lwt _closed =
        call (Cdp.Target.Close_target.command (Cdp.Target.Close_target.make_params ~target_id:created.target_id))
      in
      Cdp_lwt.Connection.close connection
    end
