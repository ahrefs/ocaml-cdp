(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the jsonkit ppx ([@@deriving json]
   plus wire attributes); equal/show/make come from ppx_deriving.

   Modules:
     Naming       protocol names -> OCaml names
     Model        the protocol as OCaml values; each type reads itself from JSON
     Protocol     file IO: read, write next to the target, remove stale files
     Hoisted_name the type name such an enum gets
     Attributes   doc comments and alerts a generated item carries
     Dependencies which domains a selection needs through $ref
     Glue         the four hand-written modules from gen/glue/, embedded at build time
     Emit         print the library code
     Sample       one JSON sample per protocol type
     Roundtrip    print the roundtrip test
     Fetch        download a protocol snapshot
   This file is only the CLI.

   Output layout:
     cdp_base.ml            -- sealed modules for every primitive alias type
                               (FrameId, TimeSinceEpoch, ...). Depends on
                               nothing, so alias references can never create
                               a cycle between domain modules.
     cdp_<domain>_types.ml  -- alias re-exports + named types. Refs to
                               primitive aliases (own or other domain) point
                               at Cdp_base. Remaining cross-domain type refs
                               are checked to form a DAG; a real cycle fails
                               generation.
     cdp_<domain>.ml        -- includes the types module; one submodule per
                               command and event (Navigate.params / .result /
                               .name). These reference only *_types modules
                               and Cdp_base, so they can never cycle.
     cdp.ml                 -- the index: Cdp.Network = Cdp_network, ...
     cdp_json.ml, cdp_command.ml, cdp_event.ml, cdp_envelope.ml
                            -- copies of the hand-written glue in gen/glue/,
                               so the output compiles on its own *)

open Cdp_gen

(* The revision is stamped into every generated header comment.
   - read from the REVISION file next to the protocol JSON
   - anything that could not be a revision id is refused *)
let read_revision ~protocol_file =
  let revision_file = Filename.concat (Filename.dirname protocol_file) "REVISION" in
  match Protocol.read_file revision_file with
  | exception Sys_error _no_revision_file -> "unknown"
  | contents ->
    let plain =
      String.length contents > 0
      && String.for_all
           (fun ch -> Naming.is_letter ch || Naming.is_digit ch || ch = '.' || ch = '_' || ch = '-')
           contents
    in
    if plain then contents
    else
      failwith
        (Printf.sprintf "cdp-gen: REVISION next to the protocol JSON holds %S, which is not a revision id" contents)

type loaded = {
  revision : string;
  selection : Model.Selection.t;
  domains : Model.Domain.t list; (* the selected ones *)
  type_index : Model.Type_index.t; (* over every loaded domain *)
}

(* shared front half of generate and roundtrip:
   read REVISION, load and select domains, build the type index, verify the refs *)
let load_protocol ~protocol_files ~domains_arg =
  let revision =
    match protocol_files with
    | first :: _others -> read_revision ~protocol_file:first
    | [] -> "unknown"
  in
  let all = List.concat_map (fun path -> Yojson.Safe.from_file path |> Model.Domain.list_of_json) protocol_files in
  Model.Domain.check_unique_names all;
  Model.Domain.check_identifiers all;
  let selection = Model.Selection.parse domains_arg in
  let domains = List.filter (fun (domain : Model.Domain.t) -> Model.Selection.contains selection domain.name) all in
  let is_loaded_domain requested =
    List.exists (fun (domain : Model.Domain.t) -> String.equal domain.name requested) all
  in
  let unknown_domains =
    match selection with
    | All -> []
    | Named names -> List.filter (fun requested -> not (is_loaded_domain requested)) names
  in
  (match unknown_domains with
  | [] -> ()
  | unknown -> failwith ("cdp-gen: unknown domains: " ^ String.concat "," unknown));
  let type_index = Model.Type_index.of_domains all in
  Dependencies.check_refs_exist ~type_index domains;
  (match Dependencies.find_missing ~all ~selected:domains with
  | [] -> ()
  | needed ->
    let who_needs =
      match selection with
      | Named [ only ] -> only ^ " also needs"
      | _several -> "the selected domains also need"
    in
    failwith (Printf.sprintf "cdp-gen: %s %s; add them to the domain list" who_needs (String.concat "," needed)));
  Dependencies.check_no_type_loop ~type_index domains;
  { revision; selection; domains; type_index }

let generate ~protocol_files ~outdir ~domains_arg =
  let { revision; selection; domains; type_index } = load_protocol ~protocol_files ~domains_arg in
  (* render every file before writing any: a generator error leaves outdir untouched *)
  let base_file = Emit.emit_base_file ~revision domains in
  let domain_files =
    List.map
      (fun (domain : Model.Domain.t) ->
        let base = Filename.concat outdir (Naming.file_of_domain domain.name) in
        ( domain,
          (base ^ "_types.ml", Emit.emit_types_file ~revision ~selected:selection ~type_index domain),
          (base ^ ".ml", Emit.emit_domain_file ~revision ~selected:selection ~type_index domain) ))
      domains
  in
  let index = Emit.emit_index ~revision domains in
  let fresh =
    "cdp_base.ml"
    :: "cdp.ml"
    :: List.concat_map
         (fun (_domain, (types_path, _types), (domain_path, _domain_contents)) ->
           [ Filename.basename types_path; Filename.basename domain_path ])
         domain_files
  in
  Protocol.remove_stale_generated_files ~outdir ~fresh;
  List.iter (fun (name, contents) -> Protocol.write_file (Filename.concat outdir name) contents) Glue.files;
  Printf.printf "wrote glue: %s\n" (String.concat ", " (List.map fst Glue.files));
  Protocol.write_file (Filename.concat outdir "cdp_base.ml") base_file;
  Printf.printf "generated cdp_base.ml: %d sealed alias modules\n" (Model.Domain.count_sealed_aliases domains);
  List.iter
    (fun ((domain : Model.Domain.t), (types_path, types_contents), (domain_path, domain_contents)) ->
      Protocol.write_file types_path types_contents;
      Protocol.write_file domain_path domain_contents;
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (Naming.file_of_domain domain.name)
        (List.length domain.types) (List.length domain.commands) (List.length domain.events))
    domain_files;
  Protocol.write_file (Filename.concat outdir "cdp.ml") index;
  Printf.printf "generated cdp.ml index (%d domains, protocol %s)\n" (List.length domains) revision

let roundtrip ~protocol_files ~outfile ~domains_arg =
  let { revision; domains; type_index; selection = _ } = load_protocol ~protocol_files ~domains_arg in
  let { Roundtrip.contents; emitted; enum_checks; skipped } = Roundtrip.emit ~revision ~domains ~type_index in
  Protocol.write_file outfile contents;
  List.iter (fun (label, reason) -> Printf.eprintf "skipped %s: %s\n" label reason) skipped;
  Printf.printf "generated %s: %d roundtrip checks, %d enum checks, %d skipped\n" outfile emitted enum_checks
    (List.length skipped)

let run action =
  try action () with
  | Failure message ->
    prerr_endline message;
    exit 1
  | Yojson.Json_error message ->
    prerr_endline ("cdp-gen: invalid protocol JSON: " ^ message);
    exit 1
  | Yojson.Safe.Util.Type_error (message, _fragment) ->
    prerr_endline ("cdp-gen: unexpected protocol shape: " ^ message);
    exit 1
  | Sys_error message ->
    prerr_endline ("cdp-gen: " ^ message);
    exit 1

let protocol_files_argument =
  let doc = "A protocol definition, usually browser_protocol.json and js_protocol.json. All of them are loaded." in
  Cmdliner.Arg.(non_empty & pos_left ~rev:true 1 non_dir_file [] & info [] ~docv:"PROTOCOL" ~doc)

let domains_argument =
  let doc = "Comma-separated domain names to generate, or $(b,all)." in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"DOMAINS" ~doc)

let generate_command =
  let doc = "Generate the typed OCaml modules for the selected domains into a directory." in
  let outdir_argument =
    let doc = "An existing directory; generated cdp_*.ml files that this run does not produce are removed from it." in
    Cmdliner.Arg.(required & pos ~rev:true 1 (some dir) None & info [] ~docv:"OUTDIR" ~doc)
  in
  let action protocol_files outdir domains_arg = run (fun () -> generate ~protocol_files ~outdir ~domains_arg) in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "generate" ~doc)
    Cmdliner.Term.(const action $ protocol_files_argument $ outdir_argument $ domains_argument)

let roundtrip_command =
  let doc = "Write the roundtrip test file for the selected domains." in
  let outfile_argument =
    let doc = "The OCaml file to write." in
    Cmdliner.Arg.(required & pos ~rev:true 1 (some string) None & info [] ~docv:"OUTFILE" ~doc)
  in
  let action protocol_files outfile domains_arg = run (fun () -> roundtrip ~protocol_files ~outfile ~domains_arg) in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "roundtrip" ~doc)
    Cmdliner.Term.(const action $ protocol_files_argument $ outfile_argument $ domains_argument)

let fetch_command =
  let doc = "Download a protocol snapshot from npm into a directory, with its LICENSE and REVISION." in
  let outdir_argument =
    let doc = "An existing directory." in
    Cmdliner.Arg.(required & pos 0 (some dir) None & info [] ~docv:"OUTDIR" ~doc)
  in
  let revision_argument =
    let doc = "A devtools-protocol revision such as 1680125; the latest when absent." in
    Cmdliner.Arg.(value & pos 1 (some string) None & info [] ~docv:"REVISION" ~doc)
  in
  let action outdir rev = run (fun () -> Fetch.fetch ~outdir ~rev) in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "fetch" ~doc) Cmdliner.Term.(const action $ outdir_argument $ revision_argument)

let () =
  let doc = "Generate typed OCaml modules from the Chrome DevTools Protocol JSON." in
  (* dune subst fills the version in at release time *)
  let info = Cmdliner.Cmd.info "cdp-gen" ~version:"%%VERSION%%" ~doc in
  exit (Cmdliner.Cmd.eval (Cmdliner.Cmd.group info [ generate_command; roundtrip_command; fetch_command ]))
