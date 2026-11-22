(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the jsonkit ppx ([@@deriving json]
   plus wire attributes); equal/show/make come from ppx_deriving.

   Modules:
     Naming       protocol names -> OCaml names
     Model        the protocol as OCaml values; each type reads itself from JSON
     Output_dir   file IO: write next to the target, remove stale files
     Hoisted_name the type name such an enum gets
     Attributes   doc comments and alerts a generated item carries
     Dependencies which domains a selection needs through $ref
     Glue         the four hand-written modules from gen/glue/, embedded at build time
     Emit         print the library code
     Sample       one JSON sample per protocol type
     Roundtrip    print the roundtrip test
     Fetch        download a protocol snapshot

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

module Protocol = struct
  type t = {
    revision : string; (* r1698617, stamped into every generated header *)
    selected_domains : Model.Domain.t list; (* only the selected ones get files *)
    type_index : Model.Type_index.t; (* over all loaded domains: a field may point into an unselected one *)
  }

  let revision_filename = "REVISION"

  let read_revision ~next_to =
    let revision_file = Filename.concat (Filename.dirname next_to) revision_filename in
    let refuse contents =
      failwith
        (Printf.sprintf "cdp-gen: %s next to the protocol JSON holds %S, which is not a revision id" revision_filename
           contents)
    in
    match Output_dir.read_file revision_file with
    | exception Sys_error _no_revision_file -> "unknown"
    | "" -> refuse ""
    | contents ->
      let is_revision_char ch =
        Naming.is_letter ch || Naming.is_digit ch || Char.equal ch '.' || Char.equal ch '_' || Char.equal ch '-'
      in
      (match String.for_all is_revision_char contents with
      | true -> contents
      | false -> refuse contents)

  let read_domains ~protocol_files =
    List.concat_map (fun path -> Yojson.Safe.from_file path |> Model.Domain.list_of_json) protocol_files

  (* keep domains the user asked for *)
  let keep_selected_domains ~selection all_domains =
    match Model.Domain_selection.find_unknown selection ~all_domains with
    | [] ->
      List.filter (fun (domain : Model.Domain.t) -> Model.Domain_selection.contains selection domain.name) all_domains
    | unknown -> failwith ("cdp-gen: unknown domains: " ^ String.concat "," unknown)

  (* every $ref resolves, the selection needs no other domain, no loop between type files.
     All named at once, so the user fixes the domain list in one try. *)
  let check_refs ~type_index ~selection ~all_domains ~selected_domains =
    Dependencies.check_refs_exist ~type_index selected_domains;
    (match Dependencies.find_missing ~all_domains ~selected_domains with
    | [] -> ()
    | needed ->
      let who_needs =
        match selection with
        | Model.Domain_selection.Named [ only ] -> only ^ " also needs"
        | _several -> "the selected domains also need"
      in
      failwith (Printf.sprintf "cdp-gen: %s %s; add them to the domain list" who_needs (String.concat "," needed)));
    Dependencies.check_no_type_loop ~type_index selected_domains

  let of_files ~protocol_files ~domains_arg =
    let revision =
      match protocol_files with
      | [] -> "unknown"
      | first_protocol_file :: _others -> read_revision ~next_to:first_protocol_file
    in
    let all_domains = read_domains ~protocol_files in
    Model.Domain.check_unique_names all_domains;
    Model.Domain.check_identifiers all_domains;
    let domain_selection = Model.Domain_selection.parse domains_arg in
    let selected_domains = keep_selected_domains ~selection:domain_selection all_domains in
    let type_index = Model.Type_index.of_domains all_domains in
    check_refs ~type_index ~selection:domain_selection ~all_domains ~selected_domains;
    { revision; selected_domains; type_index }
end

(* every file is rendered before any is written: a generator error leaves outdir untouched *)
module Library = struct
  type t = {
    revision : string;
    selected_domains : Model.Domain.t list; (* the report counts their types, commands and events *)
    glue_files : Model.Output_file.t list; (* cdp_json.ml and three more, copied from gen/glue/ *)
    base_file : Model.Output_file.t;
    domain_files : Model.Output_file.t list; (* cdp_page_types.ml and cdp_page.ml for each domain *)
    index_file : Model.Output_file.t; (* cdp.ml: lets users write Cdp.Network instead of Cdp_network *)
  }

  let base_filename = "cdp_base.ml"
  let index_filename = "cdp.ml"

  let render_base_file ~revision selected_domains =
    let contents = Emit.emit_base_file ~revision selected_domains in
    Model.Output_file.make ~name:base_filename ~contents

  let render_domain_files ~revision ~type_index selected_domains =
    let render_domain domain =
      let output : Emit.domain_output = Emit.emit_domain ~revision ~type_index domain in
      [ output.types_file; output.domain_file ]
    in
    List.concat_map render_domain selected_domains

  let render_index_file ~revision selected_domains =
    let contents = Emit.emit_index ~revision selected_domains in
    Model.Output_file.make ~name:index_filename ~contents

  let list_files library = library.glue_files @ [ library.base_file ] @ library.domain_files @ [ library.index_file ]

  let remove_stale_files ~outdir library =
    let fresh_files = list_files library in
    Output_dir.remove_stale ~outdir ~fresh:fresh_files

  let write_files ~outdir library =
    let files = list_files library in
    List.iter (Output_dir.write ~outdir) files

  let print_report ~removed_files library =
    let print_removed_file name = Printf.printf "removed stale %s\n" name in
    List.iter print_removed_file removed_files;
    let glue_names = List.map (fun (file : Model.Output_file.t) -> file.name) library.glue_files in
    let joined_glue_names = String.concat ", " glue_names in
    Printf.printf "wrote glue: %s\n" joined_glue_names;
    let sealed_alias_count = Model.Domain.count_sealed_aliases library.selected_domains in
    Printf.printf "generated %s: %d sealed alias modules\n" library.base_file.name sealed_alias_count;
    let print_domain (domain : Model.Domain.t) =
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (Naming.file_of_domain domain.name)
        (List.length domain.types) (List.length domain.commands) (List.length domain.events)
    in
    List.iter print_domain library.selected_domains;
    let domain_count = List.length library.selected_domains in
    Printf.printf "generated %s index (%d domains, protocol %s)\n" library.index_file.name domain_count library.revision

  let of_protocol { Protocol.revision; selected_domains; type_index } =
    {
      revision;
      selected_domains;
      glue_files = Glue.files;
      base_file = render_base_file ~revision selected_domains;
      domain_files = render_domain_files ~revision ~type_index selected_domains;
      index_file = render_index_file ~revision selected_domains;
    }
end

let generate ~protocol_files ~outdir ~domains_arg =
  let protocol = Protocol.of_files ~protocol_files ~domains_arg in
  let library = Library.of_protocol protocol in
  let removed_files = Library.remove_stale_files ~outdir library in
  Library.write_files ~outdir library;
  Library.print_report ~removed_files library

let roundtrip ~protocol_files ~outfile ~domains_arg =
  let { Protocol.revision; selected_domains; type_index } = Protocol.of_files ~protocol_files ~domains_arg in
  (* the same printing run as generate; only the receipt is kept, the files are not written *)
  let outputs = List.map (Emit.emit_domain ~revision ~type_index) selected_domains in
  let codecs = List.concat_map (fun (output : Emit.domain_output) -> output.codecs) outputs in
  let { Roundtrip.contents; emitted; enum_checks; skipped } = Roundtrip.emit ~revision ~type_index ~codecs in
  Output_dir.write_file outfile contents;
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
