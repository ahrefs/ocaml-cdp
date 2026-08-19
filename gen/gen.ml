(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the melange-json ppx ([@@deriving json]
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
     try read_file revision_file with Sys_error _ -> "unknown");
  let all = load_domains browser @ load_domains js in
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
  let alias_tbl = build_alias_table domains in
  check_types_dag domains ~alias_tbl;
  selected, domains, alias_tbl

let generate ~browser ~js ~outdir ~domains_arg =
  let selected, domains, alias_tbl = load_protocol ~browser ~js ~domains_arg in
  write_file (Filename.concat outdir "cdp_base.ml") (emit_base_file ~alias_tbl domains);
  Printf.printf "generated cdp_base.ml: %d sealed alias modules\n" (Hashtbl.length alias_tbl);
  List.iter
    (fun (domain : domain) ->
      let base = Filename.concat outdir (file_of_domain domain.name) in
      write_file (base ^ "_types.ml") (emit_types_file ~selected ~alias_tbl domain);
      write_file (base ^ ".ml") (emit_domain_file ~selected ~alias_tbl domain);
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (file_of_domain domain.name)
        (List.length domain.types) (List.length domain.commands) (List.length domain.events))
    domains;
  write_file (Filename.concat outdir "cdp.ml") (emit_index domains);
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
  with Failure message ->
    prerr_endline message;
    exit 1
