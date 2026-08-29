(* Launching a local headless Chrome and finding its WebSocket address.
   Chrome announces "DevTools listening on ws://..." on stderr; we start it
   with --remote-debugging-port=0 (pick any free port) and read that line. *)

(** A launched Chrome: [ws_url] is the DevTools WebSocket address to connect a transport to; [kill] terminates the
    process and removes its temporary profile directory. *)
type t = {
  ws_url : string;
  kill : unit -> unit Lwt.t;
}

(** Why {!launch} failed. [stderr] is the tail of what Chrome wrote before dying *)
type launch_error =
  | Executable_not_found of string
  | Announce_timeout of {
      timeout : float;
      stderr : string list;
    }
  | Exited_early of { stderr : string list }

exception Launch_failed of launch_error

let stderr_hint stderr =
  match stderr with
  | [] -> ""
  | lines -> "; stderr: " ^ String.concat " | " lines

let () =
  Printexc.register_printer (function
    | Launch_failed (Executable_not_found executable) ->
      Some (Printf.sprintf "Cdp_lwt.Chrome.Launch_failed: executable %S not found" executable)
    | Launch_failed (Announce_timeout { timeout; stderr }) ->
      Some
        (Printf.sprintf "Cdp_lwt.Chrome.Launch_failed: no DevTools announcement within %gs%s" timeout
           (stderr_hint stderr))
    | Launch_failed (Exited_early { stderr }) ->
      Some
        (Printf.sprintf "Cdp_lwt.Chrome.Launch_failed: chrome exited before announcing a DevTools address%s"
           (stderr_hint stderr))
    | _other_exception -> None)

let default_executable =
  match Sys.getenv_opt "CDP_CHROME" with
  | Some path -> path
  | None -> "google-chrome"

(* the default executable is a PATH name, not a path: without this pre-spawn
   check a missing binary folds into Exited_early *)
let executable_exists executable =
  match String.contains executable '/' with
  | true -> Sys.file_exists executable
  | false ->
    let path_entries = String.split_on_char ':' (Option.value ~default:"" (Sys.getenv_opt "PATH")) in
    List.exists (fun dir -> dir <> "" && Sys.file_exists (Filename.concat dir executable)) path_entries

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
    Array.iter
      (fun entry ->
        try remove_tree (Filename.concat path entry) with Unix.Unix_error (Unix.ENOENT, _entry, _gone) -> ())
      (Sys.readdir path);
    Unix.rmdir path
  | _file_or_link -> Unix.unlink path

(* Chrome logs to its pipes for its whole life; a pipe holds ~64KB, and once
   it fills, Chrome's next write BLOCKS — the entire browser freezes. So both
   pipes must be read forever, and the output discarded. *)
let rec drain channel =
  match%lwt Lwt_io.read ~count:4096 channel with
  | "" -> Lwt.return_unit (* EOF: Chrome is gone *)
  | _discarded -> drain channel
  | exception _closed_by_kill -> Lwt.return_unit

let announcement_prefix = "DevTools listening on "
let stderr_tail_limit = 10

let rec read_announcement ~recent stderr_channel =
  let%lwt line = Lwt_io.read_line stderr_channel in
  match String.length line >= String.length announcement_prefix with
  | true when String.sub line 0 (String.length announcement_prefix) = announcement_prefix ->
    Lwt.return
      (String.sub line (String.length announcement_prefix) (String.length line - String.length announcement_prefix))
  | _not_the_announcement ->
    recent := line :: List.filteri (fun index _kept -> index < stderr_tail_limit - 1) !recent;
    read_announcement ~recent stderr_channel

(** [launch ()] starts a headless Chrome with a fresh private profile and returns its DevTools WebSocket address.

    - [executable]: the Chrome binary; defaults to [$CDP_CHROME] or ["google-chrome"].
    - [timeout]: seconds to wait for the DevTools address.
    - [no_sandbox]: turn off Chrome's sandbox.
    - [port]: DevTools port; [0] (the default) picks any free port.
    - [extra_args]: appended to the Chrome command line. *)
let launch ?(executable = default_executable) ?(no_sandbox = false) ?(timeout = 15.0) ?(port = 0) ?(extra_args = []) ()
  : t Lwt.t =
  match executable_exists executable with
  | false -> Lwt.fail (Launch_failed (Executable_not_found executable))
  | true ->
    let profile_dir = create_profile_dir ~attempts_left:10 in
    let sandbox_arguments = if no_sandbox then [ "--no-sandbox" ] else [] in
    let arguments =
      [ executable; "--headless"; "--remote-debugging-port=" ^ string_of_int port ]
      @ sandbox_arguments
      @ [ "--user-data-dir=" ^ profile_dir; "about:blank" ]
      @ extra_args
    in
    let process = Lwt_process.open_process_full ("", Array.of_list arguments) in
    (* stdout is never used: drain it from the start. stderr is drained only
     after the announcement was read from it (below). *)
    Lwt.async (fun () -> drain process#stdout);
    let kill () =
      process#kill Sys.sigterm;
      let exited =
        let%lwt (_status : Unix.process_status) = Lwt.protected process#status in
        Lwt.return `Exited
      in
      let deadline =
        let%lwt () = Lwt_unix.sleep 2.0 in
        Lwt.return `Still_running
      in
      let%lwt outcome = Lwt.pick [ exited; deadline ] in
      (match outcome with
      | `Exited -> ()
      | `Still_running -> process#terminate);
      let%lwt (_status : Unix.process_status) = process#close in
      (* chrome's crashpad helper is not our child and can write into the
         profile for a moment after the browser died: retry briefly. Teardown
         must not raise; the worst case is a leftover private dir. *)
      let rec remove_profile ~attempts_left =
        match remove_tree profile_dir with
        | () -> Lwt.return_unit
        | exception (Unix.Unix_error _ | Sys_error _) ->
        match attempts_left with
        | 0 -> Lwt.return_unit
        | tries_remaining ->
          let%lwt () = Lwt_unix.sleep 0.1 in
          remove_profile ~attempts_left:(tries_remaining - 1)
      in
      remove_profile ~attempts_left:20
    in
    let recent_stderr = ref [] in
    let announcement =
      let%lwt ws_url = read_announcement ~recent:recent_stderr process#stderr in
      Lwt.return (`Announced ws_url)
    in
    let deadline =
      let%lwt () = Lwt_unix.sleep timeout in
      Lwt.return `Deadline
    in
    (match%lwt Lwt.pick [ announcement; deadline ] with
    | `Announced ws_url ->
      Lwt.async (fun () -> drain process#stderr);
      Lwt.return { ws_url; kill }
    | `Deadline ->
      let%lwt () = kill () in
      Lwt.fail (Launch_failed (Announce_timeout { timeout; stderr = List.rev !recent_stderr }))
    | exception End_of_file ->
      let%lwt () = kill () in
      Lwt.fail (Launch_failed (Exited_early { stderr = List.rev !recent_stderr }))
    | exception failure ->
      let%lwt () = kill () in
      Lwt.fail failure)

(** [with_launch f] launches like {!launch}, runs [f], and always kills the Chrome — the process is reaped and its
    profile removed also when [f] raises. *)
let with_launch ?executable ?no_sandbox ?timeout ?port ?extra_args callback =
  let%lwt chrome = launch ?executable ?no_sandbox ?timeout ?port ?extra_args () in
  Lwt.finalize (fun () -> callback chrome) (fun () -> chrome.kill ())
