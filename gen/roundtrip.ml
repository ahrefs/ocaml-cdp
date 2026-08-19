(* Emits the roundtrip test file: every generated type decodes a sample JSON
   synthesized from the protocol schema, encodes it back, decodes again, and
   compares the two values with the derived equal function. Types whose
   sample cannot be synthesized (required-field cycles) are skipped and
   reported to the caller. *)

open Naming
open Protocol

let spf = Printf.sprintf

type target = {
  label : string;
  of_json_path : string;
  to_json_path : string;
  equal_path : string;
  sample : unit -> Json.t;
}

let named_type_targets ~domains ~alias_tbl (domain : domain) =
  let index_module = module_of_domain domain.name in
  List.map
    (fun type_def ->
      let id = jstr "id" type_def in
      let label = domain.name ^ "." ^ id in
      let sample () = Sample.of_named ~domains ~visiting:[] (domain.name, id) in
      match Hashtbl.mem alias_tbl (domain.name, id) with
      | true ->
        let sealed = spf "Cdp.Base.%s.%s" index_module (submodule_of_name id) in
        {
          label;
          of_json_path = sealed ^ ".of_json";
          to_json_path = sealed ^ ".to_json";
          equal_path = sealed ^ ".equal";
          sample;
        }
      | false ->
        let type_name = sanitize_lower id in
        {
          label;
          of_json_path = spf "Cdp.%s.%s_of_json" index_module type_name;
          to_json_path = spf "Cdp.%s.%s_to_json" index_module type_name;
          equal_path = spf "Cdp.%s.equal_%s" index_module type_name;
          sample;
        })
    domain.types

let record_target ~domains ~domain_name ~submodule_path ~label ~record_name props =
  {
    label;
    of_json_path = spf "%s.%s_of_json" submodule_path record_name;
    to_json_path = spf "%s.%s_to_json" submodule_path record_name;
    equal_path = spf "%s.equal_%s" submodule_path record_name;
    sample = (fun () -> Sample.of_props ~domains ~visiting:[] ~domain:domain_name props);
  }

let item_targets ~domains ~alias_tbl (domain : domain) =
  let index_module = module_of_domain domain.name in
  Emit.item_modules ~alias_tbl domain
  |> List.concat_map (fun (mname, item) ->
    let submodule_path = spf "Cdp.%s.%s" index_module mname in
    let label = spf "%s.%s" domain.name mname in
    let params_target props =
      record_target ~domains ~domain_name:domain.name ~submodule_path ~label:(label ^ ".params") ~record_name:"params"
        props
    in
    match item with
    | `Command command ->
      let params =
        match jlist "parameters" command with
        | [] -> []
        | props -> [ params_target props ]
      in
      let result =
        match jlist "returns" command with
        | [] -> [] (* zero-return commands decode to unit; nothing to roundtrip *)
        | props ->
          [
            record_target ~domains ~domain_name:domain.name ~submodule_path ~label:(label ^ ".result")
              ~record_name:"result" props;
          ]
      in
      params @ result
    | `Event event ->
    match jlist "parameters" event with
    | [] -> []
    | props -> [ params_target props ])

(* returns (file contents, emitted count, skipped (label, reason) list) *)
let emit ~domains ~alias_tbl =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf (Emit.header ());
  Buffer.add_string buf
    "(* Roundtrip tests over every generated type: decode a sample synthesized\n\
    \   from the protocol schema, encode it back, decode again, and compare. *)\n\n\
     let failures = ref 0\n\n\
     let check name of_json to_json equal raw =\n\
    \  let json = Yojson.Basic.from_string raw in\n\
    \  let decoded = of_json json in\n\
    \  let encoded = to_json decoded in\n\
    \  let redecoded = of_json encoded in\n\
    \  match equal decoded redecoded with\n\
    \  | true -> ()\n\
    \  | false ->\n\
    \    incr failures;\n\
    \    Printf.printf \"FAIL %s: value changed after an encode/decode roundtrip\\n\" name\n\n";
  let emitted = ref 0 in
  let skipped = ref [] in
  List.iter
    (fun (domain : domain) ->
      let targets = named_type_targets ~domains ~alias_tbl domain @ item_targets ~domains ~alias_tbl domain in
      List.iter
        (fun target ->
          match target.sample () with
          | sample ->
            incr emitted;
            Buffer.add_string buf
              (spf "let () = check %S %s %s %s %S\n" target.label target.of_json_path target.to_json_path
                 target.equal_path (Json.to_string sample))
          | exception Failure reason -> skipped := (target.label, reason) :: !skipped)
        targets)
    domains;
  Buffer.add_string buf
    "\n\
     let () =\n\
    \  match !failures with\n\
    \  | 0 -> print_endline \"all roundtrip tests passed\"\n\
    \  | count -> failwith (Printf.sprintf \"%d roundtrip failures\" count)\n";
  Buffer.contents buf, !emitted, List.rev !skipped
