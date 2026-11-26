(* Downloading a protocol snapshot. Google publishes every snapshot to npm
   as devtools-protocol@0.0.<rev>, so an exact revision is one tarball away.
   Without a revision we resolve "latest" through the npm registry first,
   so REVISION is always exact.

   We shell out to curl and tar rather than depend on an OCaml HTTP + TLS
   stack: fetch is a developer-time command, and the opam precedent (opam
   downloads through system curl/wget) means every working OCaml setup has
   them. Downloads are staged as <file>.tmp inside the output directory and
   renamed into place only after every step succeeded, so a failed fetch
   never damages existing files. *)

let protocol_files = [ "browser_protocol.json"; "js_protocol.json" ]

(* the protocol JSONs are BSD-3-Clause (Chromium Authors); redistributing them
   requires shipping the upstream license text alongside, so every fetch
   refreshes it together with the JSONs *)
let license_file = "LICENSE"

module Revision : sig
  (* always digits like "1680125" *)
  type t = private string

  val of_string : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_string text =
    let is_all_digits =
      match text with
      | "" -> false
      | _not_empty -> String.for_all Naming.is_digit text
    in
    match is_all_digits with
    | true -> text
    | false -> failwith (Printf.sprintf "cdp-gen: revision must be a number (like 1680125), got %S" text)

  let to_string revision = revision
end

module Staged_file : sig
  type t = private {
    filename : string; (* "browser_protocol.json" *)
    path_in_tarball : string; (* "package/json/browser_protocol.json" *)
    temporary_path : string; (* "protocol/browser_protocol.json.tmp" *)
  }

  val make : outdir:string -> directory_in_tarball:string -> string -> t
end = struct
  type t = {
    filename : string;
    path_in_tarball : string;
    temporary_path : string;
  }

  let make ~outdir ~directory_in_tarball filename =
    {
      filename;
      path_in_tarball = Filename.concat directory_in_tarball filename;
      temporary_path = Filename.concat outdir (filename ^ ".tmp");
    }
end

let check_tool name =
  match Sys.command (Printf.sprintf "command -v %s > /dev/null 2>&1" name) with
  | 0 -> ()
  | _missing ->
    failwith
      (Printf.sprintf "cdp-gen: `%s` is required by fetch but was not found on this machine; please install it" name)

let run_command ~context command =
  match Sys.command command with
  | 0 -> ()
  | exit_code -> failwith (Printf.sprintf "cdp-gen: %s failed (exit %d): %s" context exit_code command)

let download ~url ~destination =
  run_command ~context:(Printf.sprintf "downloading %s" url)
    (Printf.sprintf "curl -sfL --connect-timeout 15 --max-time 300 %s -o %s" (Filename.quote url)
       (Filename.quote destination))

let request_latest_revision () =
  let registry_response = Filename.temp_file "cdp_gen_registry" ".json" in
  Fun.protect
    ~finally:(fun () -> Output_dir.remove_if_exists registry_response)
    (fun () ->
      let latest_version_url = "https://registry.npmjs.org/devtools-protocol/latest" in
      download ~url:latest_version_url ~destination:registry_response;
      let version =
        Yojson.Safe.Util.member "version" (Yojson.Safe.from_file registry_response) |> Yojson.Safe.Util.to_string
      in
      match String.split_on_char '.' version with
      | [ "0"; "0"; revision ] -> Revision.of_string revision
      | _unexpected_format -> failwith ("cdp-gen: unexpected npm version: " ^ version))

(* the extracted file must be a real protocol definition, not a truncated
   download or an error page *)
let check_protocol_json ~file path =
  match Yojson.Safe.from_file path with
  | exception Yojson.Json_error _parse_error ->
    failwith (Printf.sprintf "cdp-gen: downloaded %s is not valid JSON" file)
  | json ->
  match Yojson.Safe.Util.member "domains" json with
  | `List (_ :: _) -> ()
  | _no_domains -> failwith (Printf.sprintf "cdp-gen: downloaded %s does not look like a protocol definition" file)

let fetch ~outdir ~requested_revision =
  check_tool "curl";
  check_tool "tar";
  let revision =
    match requested_revision with
    | None -> request_latest_revision ()
    | Some given -> Revision.of_string given
  in
  let url =
    Printf.sprintf "https://registry.npmjs.org/devtools-protocol/-/devtools-protocol-0.0.%s.tgz"
      (Revision.to_string revision)
  in
  let tarball = Filename.temp_file "cdp_gen_protocol" ".tgz" in
  let staged_protocol_files = List.map (Staged_file.make ~outdir ~directory_in_tarball:"package/json") protocol_files in
  let staged_license = Staged_file.make ~outdir ~directory_in_tarball:"package" license_file in
  let staged_files = staged_license :: staged_protocol_files in
  Fun.protect
    ~finally:(fun () ->
      Output_dir.remove_if_exists tarball;
      List.iter
        (fun (staged_file : Staged_file.t) -> Output_dir.remove_if_exists staged_file.temporary_path)
        staged_files)
    (fun () ->
      download ~url ~destination:tarball;
      List.iter
        (fun ({ filename; path_in_tarball; temporary_path } : Staged_file.t) ->
          run_command
            ~context:(Printf.sprintf "extracting %s" filename)
            (Printf.sprintf "tar -xzOf %s %s > %s" (Filename.quote tarball) (Filename.quote path_in_tarball)
               (Filename.quote temporary_path)))
        staged_files;
      List.iter
        (fun ({ filename; path_in_tarball = _; temporary_path } : Staged_file.t) ->
          check_protocol_json ~file:filename temporary_path)
        staged_protocol_files;
      (* everything succeeded: move into place (same directory, so atomic) *)
      List.iter
        (fun ({ filename; path_in_tarball = _; temporary_path } : Staged_file.t) ->
          let final_path = Filename.concat outdir filename in
          Sys.rename temporary_path final_path)
        staged_files;
      Output_dir.write_file (Filename.concat outdir "REVISION") (Printf.sprintf "r%s\n" (Revision.to_string revision)));
  Printf.printf "fetched protocol r%s into %s/\n" (Revision.to_string revision) outdir
