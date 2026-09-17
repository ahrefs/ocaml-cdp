(* Emits the roundtrip test file: every generated type decodes a sample JSON
   synthesized from the protocol schema, encodes it back, decodes again, and
   compares the two values with the derived equal function. Types whose
   sample cannot be synthesized (required-field cycles) are skipped and
   reported to the caller. *)

open Naming
open Protocol

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
      let label = spf "%s.%s" domain.name id in
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

(* Every enum of a domain with its wire values: named enums, enums hoisted
   from a named type, enums hoisted from a command or event module. *)
type enum_target = {
  enum_label : string;
  enum_path : string; (* module path of the type, for its codecs and its Other constructor *)
  enum_type : string;
  values : string list;
}

let enum_targets ~alias_tbl (domain : domain) =
  let index_module = module_of_domain domain.name in
  let domain_path = spf "Cdp.%s" index_module in
  let hoisted_in ~enum_path ~owner ~hoist_name props =
    List.filter_map
      (fun prop ->
        let field = jstr "name" prop in
        Inline_enum.of_prop prop
        |> Option.map (fun inline_enum ->
          {
            enum_label = spf "%s.%s" owner field;
            enum_path;
            enum_type = hoist_name field;
            values = Inline_enum.values inline_enum;
          }))
      props
  in
  let named =
    List.concat_map
      (fun type_def ->
        let id = jstr "id" type_def in
        let owner = spf "%s.%s" domain.name id in
        match Util.member "enum" type_def with
        | `List values ->
          [
            {
              enum_label = owner;
              enum_path = domain_path;
              enum_type = sanitize_lower id;
              values = List.map Util.to_string values;
            };
          ]
        | _not_an_enum ->
          hoisted_in ~enum_path:domain_path ~owner ~hoist_name:(hoisted_in_type ~type_id:id)
            (jlist "properties" type_def))
      domain.types
  in
  let items =
    Emit.item_modules ~alias_tbl domain
    |> List.concat_map (fun (mname, item) ->
      let props =
        match item with
        | `Command command -> jlist "parameters" command @ jlist "returns" command
        | `Event event -> jlist "parameters" event
      in
      hoisted_in ~enum_path:(spf "%s.%s" domain_path mname) ~owner:(spf "%s.%s" domain.name mname)
        ~hoist_name:hoisted_in_item props)
  in
  named @ items

type output = {
  contents : string;
  emitted : int;
  enum_checks : int;
  skipped : (string * string) list; (* label, reason *)
}

let emit ~revision ~domains ~alias_tbl =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf (Emit.header ~revision);
  Buffer.add_string buf
    "(* Roundtrip tests over every generated type: decode a sample synthesized\n\
    \   from the protocol schema, encode it back, decode again, and compare.\n\
    \   The encoded JSON must also equal the input, keys sorted. *)\n\n\
     let failures = ref 0\n\n\
     let fail name what =\n\
    \  incr failures;\n\
    \  Printf.printf \"FAIL %s: %s\\n\" name what\n\n\
     let rec sort_keys (json : Yojson.Basic.t) : Yojson.Basic.t =\n\
    \  match json with\n\
    \  | `Assoc fields ->\n\
    \    `Assoc\n\
    \      (fields\n\
    \      |> List.sort (fun (left, _) (right, _) -> String.compare left right)\n\
    \      |> List.map (fun (key, value) -> key, sort_keys value))\n\
    \  | `List items -> `List (List.map sort_keys items)\n\
    \  | scalar -> scalar\n\n\
     let check name of_json to_json equal raw =\n\
    \  let json = Yojson.Basic.from_string raw in\n\
    \  let decoded = of_json json in\n\
    \  let encoded = to_json decoded in\n\
    \  (match Yojson.Basic.equal (sort_keys encoded) (sort_keys json) with\n\
    \  | true -> ()\n\
    \  | false -> fail name (\"encoded JSON differs from the input: \" ^ Yojson.Basic.to_string encoded));\n\
    \  let redecoded = of_json encoded in\n\
    \  match equal decoded redecoded with\n\
    \  | true -> ()\n\
    \  | false -> fail name \"value changed after an encode/decode roundtrip\"\n\n\
     (* every wire value of an enum decodes to its own constructor, never to\n\
    \   the Other fallback, and encodes back to the same string *)\n\
     let check_enum name of_json to_json is_other values =\n\
    \  List.iter\n\
    \    (fun value ->\n\
    \      let decoded = of_json (`String value) in\n\
    \      (match is_other decoded with\n\
    \      | false -> ()\n\
    \      | true -> fail name (\"value \" ^ value ^ \" decodes to Other\"));\n\
    \      match to_json decoded with\n\
    \      | `String encoded when String.equal encoded value -> ()\n\
    \      | encoded -> fail name (\"value \" ^ value ^ \" encodes back as \" ^ Yojson.Basic.to_string encoded))\n\
    \    values\n\n";
  let emitted = ref 0 in
  let enum_checks = ref 0 in
  let skipped = ref [] in
  List.iter
    (fun (domain : domain) ->
      List.iter
        (fun target ->
          incr enum_checks;
          Buffer.add_string buf
            (spf
               "let () =\n\
               \  check_enum %S %s.%s_of_json %s.%s_to_json\n\
               \    (fun (value : %s.%s) -> match value with %s.Other _ -> true | _known -> false)\n\
               \    [ %s ]\n"
               target.enum_label target.enum_path target.enum_type target.enum_path target.enum_type target.enum_path
               target.enum_type target.enum_path
               (String.concat "; " (List.map (spf "%S") target.values))))
        (enum_targets ~alias_tbl domain))
    domains;
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
  { contents = Buffer.contents buf; emitted = !emitted; enum_checks = !enum_checks; skipped = List.rev !skipped }
