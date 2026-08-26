(* Launching a local headless Chrome and finding its WebSocket address.
   Chrome announces "DevTools listening on ws://..." on stderr; we start it
   with --remote-debugging-port=0 (pick any free port) and read that line. *)

(** A launched Chrome: [ws_url] is the DevTools WebSocket address to connect a transport to; [kill] terminates the
    process and removes its temporary profile directory. *)
type t = {
  ws_url : string;
  kill : unit -> unit Lwt.t;
}

let default_executable =
  match Sys.getenv_opt "CDP_CHROME" with
  | Some path -> path
  | None -> "google-chrome"

let random_state = lazy (Random.State.make_self_init ())
let random_suffix_bound = 0x10000000
let owner_only_permissions = 0o700

(* atomic mkdir keeps the profile private on a shared temp dir and cannot be
   hijacked by a pre-created directory *)
let rec create_profile_dir ~attempts_left =
  let name = Printf.sprintf "cdp-chrome-%08x" (Random.State.int (Lazy.force random_state) random_suffix_bound) in
  let path = Filename.concat (Filename.get_temp_dir_name ()) name in
  try
    Unix.mkdir path owner_only_permissions;
    path
  with Unix.Unix_error (Unix.EEXIST, _mkdir, _path) ->
    (match attempts_left with
    | 0 -> failwith "cdp-lwt: could not create a fresh Chrome profile directory in the temp dir"
    | tries_remaining -> create_profile_dir ~attempts_left:(tries_remaining - 1))

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Array.iter (fun entry -> remove_tree (Filename.concat path entry)) (Sys.readdir path);
    Unix.rmdir path
  | _file_or_link -> Unix.unlink path

let announcement_prefix = "DevTools listening on "

let rec read_announcement stderr_channel =
  let%lwt line = Lwt_io.read_line stderr_channel in
  match String.length line >= String.length announcement_prefix with
  | true when String.sub line 0 (String.length announcement_prefix) = announcement_prefix ->
    Lwt.return
      (String.sub line (String.length announcement_prefix) (String.length line - String.length announcement_prefix))
  | _not_the_announcement -> read_announcement stderr_channel

(** [launch ()] starts a headless Chrome with a fresh private profile and returns its DevTools WebSocket address.

    - [executable]: the Chrome binary; defaults to [$CDP_CHROME] or ["google-chrome"].
    - [timeout]: seconds to wait for the DevTools address.
    - [no_sandbox]: turn off Chrome's sandbox.
    - [port]: DevTools port; [0] (the default) picks any free port.
    - [extra_args]: appended to the Chrome command line. *)
let launch ?(executable = default_executable) ?(no_sandbox = false) ?(timeout = 15.0) ?(port = 0) ?(extra_args = []) ()
  : t Lwt.t =
  let profile_dir = create_profile_dir ~attempts_left:10 in
  let sandbox_arguments = if no_sandbox then [ "--no-sandbox" ] else [] in
  let arguments =
    [ executable; "--headless"; "--remote-debugging-port=" ^ string_of_int port ]
    @ sandbox_arguments
    @ [ "--user-data-dir=" ^ profile_dir; "about:blank" ]
    @ extra_args
  in
  let process = Lwt_process.open_process_full ("", Array.of_list arguments) in
  let kill () =
    process#terminate;
    let%lwt (_status : Unix.process_status) = process#close in
    (try remove_tree profile_dir with Unix.Unix_error _ | Sys_error _ -> ());
    Lwt.return_unit
  in
  let announcement =
    let%lwt ws_url = read_announcement process#stderr in
    Lwt.return (`Announced ws_url)
  in
  let deadline =
    let%lwt () = Lwt_unix.sleep timeout in
    Lwt.return `Deadline
  in
  match%lwt Lwt.pick [ announcement; deadline ] with
  | `Announced ws_url -> Lwt.return { ws_url; kill }
  | `Deadline ->
    let%lwt () = kill () in
    Lwt.fail (Failure (Printf.sprintf "cdp-lwt: %s did not announce a DevTools address within %gs" executable timeout))
  | exception End_of_file ->
    let%lwt () = kill () in
    Lwt.fail
      (Failure
         (Printf.sprintf "cdp-lwt: %s exited before announcing a DevTools address — is it a Chrome binary?" executable))
  | exception failure ->
    let%lwt () = kill () in
    Lwt.fail failure
