(* Launching a local headless Chrome and finding its WebSocket address.
   Chrome announces "DevTools listening on ws://..." on stderr; we start it
   with --remote-debugging-port=0 (pick any free port) and read that line. *)

type t = {
  ws_url : string;
  kill : unit -> unit Lwt.t;
}

let default_executable =
  match Sys.getenv_opt "CDP_CHROME" with
  | Some path -> path
  | None -> "google-chrome"

let announcement_prefix = "DevTools listening on "

let rec read_announcement stderr_channel =
  let%lwt line = Lwt_io.read_line stderr_channel in
  match String.length line >= String.length announcement_prefix with
  | true when String.sub line 0 (String.length announcement_prefix) = announcement_prefix ->
    Lwt.return
      (String.sub line (String.length announcement_prefix) (String.length line - String.length announcement_prefix))
  | _not_the_announcement -> read_announcement stderr_channel

let launch ?(executable = default_executable) ?(no_sandbox = false) ?(extra_args = []) () : t Lwt.t =
  let profile_dir = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "cdp-chrome-%d" (Unix.getpid ())) in
  let sandbox_arguments =
    if no_sandbox then [ "--no-sandbox" ] else []
  in
  let arguments =
    [ executable; "--headless"; "--remote-debugging-port=0" ]
    @ sandbox_arguments
    @ [ "--user-data-dir=" ^ profile_dir; "about:blank" ]
    @ extra_args
  in
  let process = Lwt_process.open_process_full ("", Array.of_list arguments) in
  let kill () =
    process#terminate;
    let%lwt (_status : Unix.process_status) = process#close in
    Lwt.return_unit
  in
  let announcement =
    let%lwt ws_url = read_announcement process#stderr in
    Lwt.return { ws_url; kill }
  in
  let gave_up =
    let%lwt () = Lwt_unix.sleep 15.0 in
    let%lwt () = kill () in
    Lwt.fail (Failure ("cdp-lwt: " ^ executable ^ " did not announce a DevTools address within 15s"))
  in
  Lwt.pick [ announcement; gave_up ]
