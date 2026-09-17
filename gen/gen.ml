(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the jsonkit ppx ([@@deriving json]
   plus wire attributes); equal/show/make come from ppx_deriving.

   Modules:
     Naming     protocol names -> OCaml names
     Protocol   load and check the JSON, alias table, file IO
     Emit       print the library code
     Sample     one JSON sample per protocol type
     Roundtrip  print the roundtrip test
     Fetch      download a protocol snapshot
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
     cdp.ml                 -- the index: Cdp.Network = Cdp_network, ... *)

open Cdp_gen

(* The revision is stamped into every generated header comment.
   - read from the REVISION file next to the protocol JSON
   - anything that could not be a revision id is refused *)
let read_revision ~browser =
  let revision_file = Filename.concat (Filename.dirname browser) "REVISION" in
  match Protocol.read_file revision_file with
  | exception Sys_error _no_revision_file -> "unknown"
  | contents ->
    let plain =
      String.length contents > 0
      && String.for_all
           (fun ch -> Protocol.is_letter ch || Protocol.is_digit ch || ch = '.' || ch = '_' || ch = '-')
           contents
    in
    if plain then contents
    else
      failwith
        (Printf.sprintf "cdp-gen: REVISION next to the protocol JSON holds %S, which is not a revision id" contents)

type loaded = {
  revision : string;
  selected : string list;
  domains : Protocol.domain list;
  alias_tbl : (string * string, string) Hashtbl.t;
}

(* shared front half of generate and roundtrip: read REVISION, load and
   select domains, build the alias table, verify the type graph *)
let load_protocol ~browser ~js ~domains_arg =
  let revision = read_revision ~browser in
  let all = Protocol.load_domains browser @ Protocol.load_domains js in
  Protocol.check_unique_names all;
  Protocol.check_identifiers all;
  let selected =
    match domains_arg with
    | "all" -> List.map (fun (domain : Protocol.domain) -> domain.name) all
    | names -> String.split_on_char ',' names
  in
  let domains = List.filter (fun (domain : Protocol.domain) -> List.mem domain.name selected) all in
  (match
     List.filter
       (fun requested -> not (List.exists (fun (domain : Protocol.domain) -> String.equal domain.name requested) all))
       selected
   with
  | [] -> ()
  | missing -> failwith ("cdp-gen: unknown domains: " ^ String.concat "," missing));
  Protocol.check_refs_exist ~all ~selected:domains;
  let alias_tbl = Protocol.build_alias_table domains in
  Protocol.check_types_dag domains ~alias_tbl;
  { revision; selected; domains; alias_tbl }

let generate ~browser ~js ~outdir ~domains_arg =
  let { revision; selected; domains; alias_tbl } = load_protocol ~browser ~js ~domains_arg in
  (* render every file before writing any: a generator error leaves outdir untouched *)
  let base_file = Emit.emit_base_file ~revision ~alias_tbl domains in
  let domain_files =
    List.map
      (fun (domain : Protocol.domain) ->
        let base = Filename.concat outdir (Naming.file_of_domain domain.name) in
        ( domain,
          (base ^ "_types.ml", Emit.emit_types_file ~revision ~selected ~alias_tbl domain),
          (base ^ ".ml", Emit.emit_domain_file ~revision ~selected ~alias_tbl domain) ))
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
  Protocol.write_file (Filename.concat outdir "cdp_base.ml") base_file;
  Printf.printf "generated cdp_base.ml: %d sealed alias modules\n" (Hashtbl.length alias_tbl);
  List.iter
    (fun ((domain : Protocol.domain), (types_path, types_contents), (domain_path, domain_contents)) ->
      Protocol.write_file types_path types_contents;
      Protocol.write_file domain_path domain_contents;
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (Naming.file_of_domain domain.name)
        (List.length domain.types) (List.length domain.commands) (List.length domain.events))
    domain_files;
  Protocol.write_file (Filename.concat outdir "cdp.ml") index;
  Printf.printf "generated cdp.ml index (%d domains, protocol %s)\n" (List.length domains) revision

let roundtrip ~browser ~js ~outfile ~domains_arg =
  let { revision; domains; alias_tbl; selected = _ } = load_protocol ~browser ~js ~domains_arg in
  let { Roundtrip.contents; emitted; skipped } = Roundtrip.emit ~revision ~domains ~alias_tbl in
  Protocol.write_file outfile contents;
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
