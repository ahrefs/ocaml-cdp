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

let curl ~url ~out =
  run_command ~context:(Printf.sprintf "downloading %s" url)
    (Printf.sprintf "curl -sfL --connect-timeout 15 --max-time 300 %s -o %s" (Filename.quote url) (Filename.quote out))

let resolve_latest_revision () =
  let registry_response = Filename.temp_file "cdp_gen_registry" ".json" in
  Fun.protect
    ~finally:(fun () -> Output_dir.remove_if_exists registry_response)
    (fun () ->
      let latest_version_url = "https://registry.npmjs.org/devtools-protocol/latest" in
      curl ~url:latest_version_url ~out:registry_response;
      let version =
        Yojson.Safe.Util.member "version" (Yojson.Safe.from_file registry_response) |> Yojson.Safe.Util.to_string
      in
      match String.split_on_char '.' version with
      | [ "0"; "0"; revision ] -> revision
      | _unexpected_format -> failwith ("cdp-gen: unexpected npm version: " ^ version))

let is_all_digits text = String.length text > 0 && String.for_all Naming.is_digit text

(* the extracted file must be a real protocol definition, not a truncated
   download or an error page *)
let validate_protocol_json ~file path =
  let json =
    try Yojson.Safe.from_file path
    with Yojson.Json_error _parse_error -> failwith (Printf.sprintf "cdp-gen: downloaded %s is not valid JSON" file)
  in
  match Yojson.Safe.Util.member "domains" json with
  | `List (_ :: _) -> ()
  | _no_domains -> failwith (Printf.sprintf "cdp-gen: downloaded %s does not look like a protocol definition" file)

let fetch ~outdir ~requested_revision =
  check_tool "curl";
  check_tool "tar";
  let revision =
    match requested_revision with
    | None -> resolve_latest_revision ()
    | Some given ->
    match is_all_digits given with
    | true -> given
    | false -> failwith (Printf.sprintf "cdp-gen: revision must be a number (like 1680125), got %S" given)
  in
  let url = Printf.sprintf "https://registry.npmjs.org/devtools-protocol/-/devtools-protocol-0.0.%s.tgz" revision in
  let tarball = Filename.temp_file "cdp_gen_protocol" ".tgz" in
  (* pair each protocol file with the temporary path it is downloaded to *)
  let staged = List.map (fun file -> file, Filename.concat outdir (file ^ ".tmp")) protocol_files in
  let license_temporary_path = Filename.concat outdir (license_file ^ ".tmp") in
  Fun.protect
    ~finally:(fun () ->
      Output_dir.remove_if_exists tarball;
      Output_dir.remove_if_exists license_temporary_path;
      List.iter (fun (_file, temporary_path) -> Output_dir.remove_if_exists temporary_path) staged)
    (fun () ->
      curl ~url ~out:tarball;
      List.iter
        (fun (file, temporary_path) ->
          run_command ~context:(Printf.sprintf "extracting %s" file)
            (Printf.sprintf "tar -xzOf %s package/json/%s > %s" (Filename.quote tarball) file
               (Filename.quote temporary_path));
          validate_protocol_json ~file temporary_path)
        staged;
      run_command
        ~context:(Printf.sprintf "extracting %s" license_file)
        (Printf.sprintf "tar -xzOf %s package/%s > %s" (Filename.quote tarball) license_file
           (Filename.quote license_temporary_path));
      (* everything succeeded: move into place (same directory, so atomic) *)
      List.iter
        (fun (file, temporary_path) ->
          let final_path = Filename.concat outdir file in
          Sys.rename temporary_path final_path)
        staged;
      Sys.rename license_temporary_path (Filename.concat outdir license_file);
      Output_dir.write_file (Filename.concat outdir "REVISION") (Printf.sprintf "r%s\n" revision));
  Printf.printf "fetched protocol r%s into %s/\n" revision outdir
