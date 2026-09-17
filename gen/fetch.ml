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

open Protocol

let protocol_files = [ "browser_protocol.json"; "js_protocol.json" ]

(* the protocol JSONs are BSD-3-Clause (Chromium Authors); redistributing them
   requires shipping the upstream license text alongside, so every fetch
   refreshes it together with the JSONs *)
let license_file = "LICENSE"

let check_tool name =
  match Sys.command (spf "command -v %s > /dev/null 2>&1" name) with
  | 0 -> ()
  | _missing ->
    failwith (spf "cdp-gen: `%s` is required by fetch but was not found on this machine; please install it" name)

let run_cmd ~context cmd =
  match Sys.command cmd with
  | 0 -> ()
  | exit_code -> failwith (spf "cdp-gen: %s failed (exit %d): %s" context exit_code cmd)

let curl ~url ~out =
  run_cmd ~context:(spf "downloading %s" url)
    (spf "curl -sfL --connect-timeout 15 --max-time 300 %s -o %s" (Filename.quote url) (Filename.quote out))

let resolve_latest_revision () =
  let tmp = Filename.temp_file "cdp_gen_registry" ".json" in
  Fun.protect
    ~finally:(fun () -> remove_if_exists tmp)
    (fun () ->
      curl ~url:"https://registry.npmjs.org/devtools-protocol/latest" ~out:tmp;
      let version = Util.member "version" (Json.from_file tmp) |> Util.to_string in
      match String.split_on_char '.' version with
      | [ "0"; "0"; revision ] -> revision
      | _unexpected_format -> failwith ("cdp-gen: unexpected npm version: " ^ version))

let is_all_digits text = String.length text > 0 && String.for_all is_digit text

(* the extracted file must be a real protocol definition, not a truncated
   download or an error page *)
let validate_protocol_json ~file path =
  let json =
    try Json.from_file path
    with Yojson.Json_error _parse_error -> failwith (spf "cdp-gen: downloaded %s is not valid JSON" file)
  in
  match Util.member "domains" json with
  | `List (_ :: _) -> ()
  | _no_domains -> failwith (spf "cdp-gen: downloaded %s does not look like a protocol definition" file)

let fetch ~outdir ~rev =
  check_tool "curl";
  check_tool "tar";
  let revision =
    match rev with
    | None -> resolve_latest_revision ()
    | Some given ->
    match is_all_digits given with
    | true -> given
    | false -> failwith (spf "cdp-gen: revision must be a number (like 1680125), got %S" given)
  in
  let url = spf "https://registry.npmjs.org/devtools-protocol/-/devtools-protocol-0.0.%s.tgz" revision in
  let tarball = Filename.temp_file "cdp_gen_protocol" ".tgz" in
  (* pair each protocol file with the temporary path it is downloaded to *)
  let staged = List.map (fun file -> file, Filename.concat outdir (file ^ ".tmp")) protocol_files in
  let license_tmp = Filename.concat outdir (license_file ^ ".tmp") in
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists tarball;
      remove_if_exists license_tmp;
      List.iter (fun (_file, tmp) -> remove_if_exists tmp) staged)
    (fun () ->
      curl ~url ~out:tarball;
      List.iter
        (fun (file, tmp) ->
          run_cmd ~context:(spf "extracting %s" file)
            (spf "tar -xzOf %s package/json/%s > %s" (Filename.quote tarball) file (Filename.quote tmp));
          validate_protocol_json ~file tmp)
        staged;
      run_cmd ~context:(spf "extracting %s" license_file)
        (spf "tar -xzOf %s package/%s > %s" (Filename.quote tarball) license_file (Filename.quote license_tmp));
      (* everything succeeded: move into place (same directory, so atomic) *)
      List.iter (fun (file, tmp) -> Sys.rename tmp (Filename.concat outdir file)) staged;
      Sys.rename license_tmp (Filename.concat outdir license_file);
      write_file (Filename.concat outdir "REVISION") (spf "r%s\n" revision));
  Printf.printf "fetched protocol r%s into %s/\n" revision outdir
