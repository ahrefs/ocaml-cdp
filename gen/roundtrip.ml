(* Prints the roundtrip test: every generated type must survive decode, encode, decode.
   - one check per type, params and result, on a sample built from the schema
   - one check per enum, over every wire value
   - a type whose sample cannot be built (required-field cycle) is skipped and reported *)

(* one generated type to check: where its codecs live and how to build its sample *)
module Target = struct
  type t = {
    label : string;
    of_json_path : string;
    to_json_path : string;
    equal_path : string;
    sample : unit -> Yojson.Safe.t;
  }

  (* a sealed alias is reached through Cdp.Base, every other type through its domain module *)
  let of_type_def ~type_index ~domain_name (type_def : Model.Type_def.t) =
    let index_module = Naming.module_of_domain domain_name in
    let label = Printf.sprintf "%s.%s" domain_name type_def.id in
    let type_ref = { Model.Type_ref.domain = domain_name; id = type_def.id } in
    let sample () = Sample.of_named ~type_index ~visiting:[] type_ref in
    match Model.Type_def.sealed_primitive_of type_def with
    | Some _primitive ->
      let sealed = Printf.sprintf "Cdp.Base.%s.%s" index_module (Naming.submodule_of_name type_def.id) in
      {
        label;
        of_json_path = sealed ^ ".of_json";
        to_json_path = sealed ^ ".to_json";
        equal_path = sealed ^ ".equal";
        sample;
      }
    | None ->
      let type_name = Naming.sanitize_lower type_def.id in
      {
        label;
        of_json_path = Printf.sprintf "Cdp.%s.%s_of_json" index_module type_name;
        to_json_path = Printf.sprintf "Cdp.%s.%s_to_json" index_module type_name;
        equal_path = Printf.sprintf "Cdp.%s.equal_%s" index_module type_name;
        sample;
      }

  let of_record ~type_index ~submodule_path ~label ~record_name fields =
    {
      label;
      of_json_path = Printf.sprintf "%s.%s_of_json" submodule_path record_name;
      to_json_path = Printf.sprintf "%s.%s_to_json" submodule_path record_name;
      equal_path = Printf.sprintf "%s.equal_%s" submodule_path record_name;
      sample = (fun () -> Sample.of_properties ~type_index ~visiting:[] fields);
    }
end

module Enum_target = struct
  type t = {
    label : string;
    path : string; (* module path of the type, for its codecs and its Other constructor *)
    type_name : string;
    values : string list;
  }

  let of_field ~path ~owner ~hoist_name (field : Model.Property.t) =
    match field.shape with
    | Typed _type_expr -> None
    | Enum inline_enum ->
      Some
        {
          label = Printf.sprintf "%s.%s" owner field.name;
          path;
          type_name = hoist_name field.name;
          values = Model.Inline_enum.to_values inline_enum;
        }

  (* a named enum, or the enums hoisted out of a record's fields *)
  let of_type_def ~domain_name ~domain_path (type_def : Model.Type_def.t) =
    let owner = Printf.sprintf "%s.%s" domain_name type_def.id in
    match type_def.shape with
    | Enum values -> [ { label = owner; path = domain_path; type_name = Naming.sanitize_lower type_def.id; values } ]
    | Enum_list values ->
      let type_name = Hoisted_name.name_for_array_item ~type_id:type_def.id in
      [ { label = owner; path = domain_path; type_name; values } ]
    | Record fields ->
      let hoist_name = Hoisted_name.name_for_type_field ~type_id:type_def.id in
      List.filter_map (of_field ~path:domain_path ~owner ~hoist_name) fields
    | Sealed_alias _ | Alias _ -> []

  (* the enums hoisted out of a command's params and result, or an event's params *)
  let of_item ~domain_name ~domain_path ~domain_type_names (item : Model.Item.t) =
    let fields =
      match item.kind with
      | Command command -> command.params @ command.returns
      | Event event -> event.params
    in
    let hoist_name = Hoisted_name.name_for_item_field ~domain_type_names ~item_name:item.module_name in
    let path = Printf.sprintf "%s.%s" domain_path item.module_name in
    let owner = Printf.sprintf "%s.%s" domain_name item.module_name in
    List.filter_map (of_field ~path ~owner ~hoist_name) fields
end

let collect_type_targets ~type_index (domain : Model.Domain.t) =
  List.map (Target.of_type_def ~type_index ~domain_name:domain.name) domain.types

let collect_item_targets ~type_index (domain : Model.Domain.t) =
  let index_module = Naming.module_of_domain domain.name in
  let targets_of_item (item : Model.Item.t) =
    let submodule_path = Printf.sprintf "Cdp.%s.%s" index_module item.module_name in
    let label = Printf.sprintf "%s.%s" domain.name item.module_name in
    let make_params_target fields =
      Target.of_record ~type_index ~submodule_path ~label:(label ^ ".params") ~record_name:"params" fields
    in
    let make_result_target fields =
      Target.of_record ~type_index ~submodule_path ~label:(label ^ ".result") ~record_name:"result" fields
    in
    match item.kind with
    | Command command ->
      let params =
        match command.params with
        | [] -> []
        | fields -> [ make_params_target fields ]
      in
      let result =
        match command.returns with
        | [] -> [] (* zero-return commands decode to unit; nothing to roundtrip *)
        | fields -> [ make_result_target fields ]
      in
      params @ result
    | Event event ->
    match event.params with
    | [] -> []
    | fields -> [ make_params_target fields ]
  in
  List.concat_map targets_of_item (Model.Item.of_domain domain)

let collect_enum_targets (domain : Model.Domain.t) =
  let domain_name = domain.name in
  let domain_path = Printf.sprintf "Cdp.%s" (Naming.module_of_domain domain_name) in
  let domain_type_names = Hoisted_name.collect_domain_type_names domain in
  let enums_of_types = List.concat_map (Enum_target.of_type_def ~domain_name ~domain_path) domain.types in
  let enums_of_commands_and_events =
    List.concat_map (Enum_target.of_item ~domain_name ~domain_path ~domain_type_names) (Model.Item.of_domain domain)
  in
  enums_of_types @ enums_of_commands_and_events

type output = {
  contents : string;
  emitted : int;
  enum_checks : int;
  skipped : (string * string) list; (* label, reason *)
}

let emit ~revision ~domains ~type_index =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf (Emit.render_header ~revision);
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
  let add_enum_check { Enum_target.label; path; type_name; values } =
    incr enum_checks;
    let quoted_values = String.concat "; " (List.map (Printf.sprintf "%S") values) in
    Buffer.add_string buf
      (Printf.sprintf
         "let () =\n\
         \  check_enum %S %s.%s_of_json %s.%s_to_json\n\
         \    (fun (value : %s.%s) -> match value with %s.Other _ -> true | _known -> false)\n\
         \    [ %s ]\n"
         label path type_name path type_name path type_name path quoted_values)
  in
  let add_check (target : Target.t) =
    match target.sample () with
    | sample ->
      incr emitted;
      let raw = Yojson.Safe.to_string sample in
      Buffer.add_string buf
        (Printf.sprintf "let () = check %S %s %s %s %S\n" target.label target.of_json_path target.to_json_path
           target.equal_path raw)
    | exception Failure reason -> skipped := (target.label, reason) :: !skipped
  in
  List.iter (fun domain -> List.iter add_enum_check (collect_enum_targets domain)) domains;
  List.iter
    (fun domain ->
      List.iter add_check (collect_type_targets ~type_index domain @ collect_item_targets ~type_index domain))
    domains;
  Buffer.add_string buf
    "\n\
     let () =\n\
    \  match !failures with\n\
    \  | 0 -> print_endline \"all roundtrip tests passed\"\n\
    \  | count -> failwith (Printf.sprintf \"%d roundtrip failures\" count)\n";
  { contents = Buffer.contents buf; emitted = !emitted; enum_checks = !enum_checks; skipped = List.rev !skipped }
