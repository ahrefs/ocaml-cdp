(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the jsonkit ppx ([@@deriving json]
   plus wire attributes); equal/show/make come from ppx_deriving.

   Modules: Naming (protocol names -> OCaml names), Protocol (JSON loading,
   alias table, cycle check), Emit (printing OCaml), Fetch (downloading
   protocol snapshots). This file is only the CLI.

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
     cdp.ml                 -- the index: Cdp.Network = Cdp_network, ... *)

open Cdp_gen
open Naming
open Protocol
open Emit

(* shared front half of generate and roundtrip: read REVISION, load and
   select domains, build the alias table, verify the type graph *)
let load_protocol ~browser ~js ~domains_arg =
  (revision :=
     let revision_file = Filename.concat (Filename.dirname browser) "REVISION" in
     match read_file revision_file with
     | contents ->
       (* the revision is stamped into every generated header comment; refuse
          anything that could not be a revision id *)
       let plain =
         contents <> ""
         && String.for_all (fun ch -> is_letter ch || is_digit ch || ch = '.' || ch = '_' || ch = '-') contents
       in
       if plain then contents
       else failwith (spf "cdp-gen: REVISION next to the protocol JSON holds %S, which is not a revision id" contents)
     | exception Sys_error _no_revision_file -> "unknown");
  let all = load_domains browser @ load_domains js in
  check_unique_names all;
  check_identifiers all;
  let selected =
    match domains_arg with
    | "all" -> List.map (fun (domain : domain) -> domain.name) all
    | names -> String.split_on_char ',' names
  in
  let domains = List.filter (fun (domain : domain) -> List.mem domain.name selected) all in
  (match
     List.filter (fun requested -> not (List.exists (fun (domain : domain) -> domain.name = requested) all)) selected
   with
  | [] -> ()
  | missing -> failwith ("cdp-gen: unknown domains: " ^ String.concat "," missing));
  check_refs_exist ~all ~selected:domains;
  let alias_tbl = build_alias_table domains in
  check_types_dag domains ~alias_tbl;
  selected, domains, alias_tbl

let generate ~browser ~js ~outdir ~domains_arg =
  let selected, domains, alias_tbl = load_protocol ~browser ~js ~domains_arg in
  (* failure halfway through must not leave the output directory with a half-new, half-old mix *)
  let base_file = emit_base_file ~alias_tbl domains in
  let domain_files =
    List.map
      (fun (domain : domain) ->
        let base = Filename.concat outdir (file_of_domain domain.name) in
        ( domain,
          (base ^ "_types.ml", emit_types_file ~selected ~alias_tbl domain),
          (base ^ ".ml", emit_domain_file ~selected ~alias_tbl domain) ))
      domains
  in
  let index = emit_index domains in
  let fresh =
    "cdp_base.ml"
    :: "cdp.ml"
    :: List.concat_map
         (fun (_domain, (types_path, _types), (domain_path, _domain_contents)) ->
           [ Filename.basename types_path; Filename.basename domain_path ])
         domain_files
  in
  remove_stale_generated_files ~outdir ~fresh;
  write_file (Filename.concat outdir "cdp_base.ml") base_file;
  Printf.printf "generated cdp_base.ml: %d sealed alias modules\n" (Hashtbl.length alias_tbl);
  List.iter
    (fun ((domain : domain), (types_path, types_contents), (domain_path, domain_contents)) ->
      write_file types_path types_contents;
      write_file domain_path domain_contents;
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (file_of_domain domain.name)
        (List.length domain.types) (List.length domain.commands) (List.length domain.events))
    domain_files;
  write_file (Filename.concat outdir "cdp.ml") index;
  Printf.printf "generated cdp.ml index (%d domains, protocol %s)\n" (List.length domains) !revision

let roundtrip ~browser ~js ~outfile ~domains_arg =
  let _selected, domains, alias_tbl = load_protocol ~browser ~js ~domains_arg in
  let contents, emitted, skipped = Roundtrip.emit ~domains ~alias_tbl in
  write_file outfile contents;
  List.iter (fun (label, reason) -> Printf.eprintf "skipped %s: %s\n" label reason) skipped;
  Printf.printf "generated %s: %d roundtrip checks, %d skipped\n" outfile emitted (List.length skipped)

let usage =
  "usage:\n\
  \  cdp-gen generate <browser_protocol.json> <js_protocol.json> <outdir> <Domain1,Domain2,...|all>\n\
  \  cdp-gen roundtrip <browser_protocol.json> <js_protocol.json> <outfile.ml> <Domain1,Domain2,...|all>\n\
  \  cdp-gen fetch <outdir> [<revision>]\n\
   generate stamps headers from the REVISION file next to the protocol JSON."

let () =
  try
    match Array.to_list Sys.argv with
    | _ :: "generate" :: [ browser; js; outdir; domains ] -> generate ~browser ~js ~outdir ~domains_arg:domains
    | _ :: "roundtrip" :: [ browser; js; outfile; domains ] -> roundtrip ~browser ~js ~outfile ~domains_arg:domains
    | _ :: "fetch" :: [ outdir ] -> Fetch.fetch ~outdir ~rev:None
    | _ :: "fetch" :: [ outdir; rev ] -> Fetch.fetch ~outdir ~rev:(Some rev)
    | _invalid_arguments ->
      prerr_endline usage;
      exit 1
  with
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
