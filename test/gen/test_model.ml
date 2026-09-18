(* Model.Domain.of_json must see the protocol exactly as the JSON-reading code does,
   or moving Emit onto the model would change the generated files.
   - the real protocol: same domains, counts, sealed aliases and inline enums
   - hand-written shapes: the corner cases of the type-def kinds
   - the shape errors, with their pinned messages *)

let pass name = Printf.printf "PASS %s\n" name

let expect_failure ~name ~message run =
  match run () with
  | exception Failure actual when String.equal actual message -> pass name
  | exception Failure actual -> failwith (Printf.sprintf "%s: wrong message %S" name actual)
  | _unexpected_success -> failwith ("expected a failure, got a result: " ^ name)

let protocol_files = [ "../../protocol/browser_protocol.json"; "../../protocol/js_protocol.json" ]

let json_domains = List.concat_map Cdp_gen.Protocol.load_domains protocol_files

let model_domains =
  List.concat_map (fun path -> Yojson.Safe.from_file path |> Cdp_gen.Model.Domain.list_of_json) protocol_files

let () =
  assert (Int.equal (List.length json_domains) (List.length model_domains));
  List.iter2
    (fun (json_domain : Cdp_gen.Protocol.domain) (model_domain : Cdp_gen.Model.Domain.t) ->
      assert (String.equal json_domain.name model_domain.name);
      assert (Int.equal (List.length json_domain.types) (List.length model_domain.types));
      assert (Int.equal (List.length json_domain.commands) (List.length model_domain.commands));
      assert (Int.equal (List.length json_domain.events) (List.length model_domain.events)))
    json_domains model_domains;
  pass (Printf.sprintf "%d domains: same names and counts" (List.length model_domains))

let () =
  (* the old table is keyed by a (domain, id) pair *)
  let alias_table = Cdp_gen.Protocol.build_alias_table json_domains in
  let sealed_keys =
    List.concat_map
      (fun (domain : Cdp_gen.Model.Domain.t) ->
        List.filter_map
          (fun (type_def : Cdp_gen.Model.Type_def.t) ->
            match type_def.shape with
            | Sealed_alias _primitive -> Some (domain.name, type_def.id)
            | _other_shape -> None)
          domain.types)
      model_domains
  in
  assert (Int.equal (List.length sealed_keys) (Hashtbl.length alias_table));
  List.iter (fun key -> assert (Hashtbl.mem alias_table key)) sealed_keys;
  pass (Printf.sprintf "%d sealed aliases match the alias table" (List.length sealed_keys))

let collect_json_properties (domain : Cdp_gen.Protocol.domain) =
  List.concat_map (Cdp_gen.Protocol.get_list "properties") domain.types
  @ List.concat_map
      (fun command -> Cdp_gen.Protocol.get_list "parameters" command @ Cdp_gen.Protocol.get_list "returns" command)
      domain.commands
  @ List.concat_map (Cdp_gen.Protocol.get_list "parameters") domain.events

let collect_model_properties (domain : Cdp_gen.Model.Domain.t) =
  List.concat_map
    (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Record fields -> fields
      | _no_fields -> [])
    domain.types
  @ List.concat_map (fun (command : Cdp_gen.Model.Command.t) -> command.params @ command.returns) domain.commands
  @ List.concat_map (fun (event : Cdp_gen.Model.Event.t) -> event.params) domain.events

let () =
  let all_json_properties = List.concat_map collect_json_properties json_domains in
  let all_model_properties = List.concat_map collect_model_properties model_domains in
  assert (Int.equal (List.length all_json_properties) (List.length all_model_properties));
  let json_inline_enums =
    List.filter (fun property -> Option.is_some (Cdp_gen.Inline_enum.of_prop property)) all_json_properties
  in
  let model_inline_enums =
    List.filter
      (fun (property : Cdp_gen.Model.Property.t) ->
        match property.shape with
        | Enum _inline_enum -> true
        | Typed _type_expr -> false)
      all_model_properties
  in
  assert (Int.equal (List.length json_inline_enums) (List.length model_inline_enums));
  let all_count = List.length all_model_properties in
  let inline_enum_count = List.length model_inline_enums in
  pass (Printf.sprintf "%d properties, %d with an inline enum" all_count inline_enum_count)

let () =
  let count shape_name matches =
    let total =
      List.length
        (List.concat_map (fun (domain : Cdp_gen.Model.Domain.t) -> List.filter matches domain.types) model_domains)
    in
    Printf.printf "  %-13s %d\n" shape_name total;
    total
  in
  let sealed =
    count "Sealed_alias" (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Sealed_alias _primitive -> true
      | _other -> false)
  in
  let enums =
    count "Enum" (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Enum _values -> true
      | _other -> false)
  in
  let enum_lists =
    count "Enum_list" (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Enum_list _values -> true
      | _other -> false)
  in
  let records =
    count "Record" (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Record _fields -> true
      | _other -> false)
  in
  let aliases =
    count "Alias" (fun (type_def : Cdp_gen.Model.Type_def.t) ->
      match type_def.shape with
      | Alias _type_expr -> true
      | _other -> false)
  in
  let json_enum_types =
    List.length
      (List.concat_map
         (fun (domain : Cdp_gen.Protocol.domain) ->
           List.filter
             (fun type_def ->
               match Yojson.Safe.Util.member "enum" type_def with
               | `List _values -> true
               | _no_enum -> false)
             domain.types)
         json_domains)
  in
  assert (Int.equal enums json_enum_types);
  let all_types = List.length (List.concat_map (fun (domain : Cdp_gen.Model.Domain.t) -> domain.types) model_domains) in
  assert (Int.equal all_types (sealed + enums + enum_lists + records + aliases));
  pass (Printf.sprintf "%d named types, one kind each" all_types)

let parse_types raw =
  let domain =
    Cdp_gen.Model.Domain.of_json (Yojson.Safe.from_string (Printf.sprintf {|{"domain":"Demo","types":[%s]}|} raw))
  in
  domain.types

let parse_only_type raw =
  match parse_types raw with
  | [ type_def ] -> type_def
  | _not_one -> failwith "expected one type"

let () =
  (match (parse_only_type {|{"id":"Raw","type":"object","properties":[]}|}).shape with
  | Alias Any -> ()
  | _other -> failwith "empty properties must read as Alias Any");
  (match (parse_only_type {|{"id":"Blob","type":"binary"}|}).shape with
  | Alias Binary -> ()
  | _other -> failwith "binary is not a sealed alias");
  (match (parse_only_type {|{"id":"Modes","type":"array","items":{"type":"string","enum":["a","b"]}}|}).shape with
  | Enum_list [ "a"; "b" ] -> ()
  | _other -> failwith "an array of an inline enum is an Enum_list");
  (match (parse_only_type {|{"id":"Ids","type":"array","items":{"$ref":"Other.Id"}}|}).shape with
  | Alias (Array (Ref { domain = "Other"; id = "Id" })) -> ()
  | _other -> failwith "a qualified $ref keeps its domain");
  (match (parse_only_type {|{"id":"Local","type":"array","items":{"$ref":"Point"}}|}).shape with
  | Alias (Array (Ref { domain = "Demo"; id = "Point" })) -> ()
  | _other -> failwith "a bare $ref means the current domain");
  (match (parse_only_type {|{"id":"Mood","type":"string","enum":["happy","sad"]}|}).shape with
  | Enum [ "happy"; "sad" ] -> ()
  | _other -> failwith "a named enum");
  (match
     (parse_only_type {|{"id":"Point","type":"object","properties":[{"name":"x","type":"integer","optional":true}]}|})
       .shape
   with
  | Record [ { name = "x"; shape = Typed (Primitive Integer); optional = true; _ } ] -> ()
  | _other -> failwith "a record field");
  pass "type-def kinds on hand-written shapes"

let () =
  expect_failure ~name:"a field with both $ref and type"
    ~message:{|cdp-gen: Demo.Thing: field "a" has both $ref and type|} (fun () ->
    parse_types {|{"id":"Thing","type":"object","properties":[{"name":"a","$ref":"Frame","type":"integer"}]}|});
  expect_failure ~name:"a field with both $ref and enum"
    ~message:{|cdp-gen: Demo.Thing: field "b" has both $ref and enum|} (fun () ->
    parse_types {|{"id":"Thing","type":"object","properties":[{"name":"b","$ref":"Frame","enum":["x"]}]}|});
  expect_failure ~name:"a field with neither" ~message:{|cdp-gen: Demo.Thing: field "c" has neither type nor $ref|}
    (fun () -> parse_types {|{"id":"Thing","type":"object","properties":[{"name":"c","optional":true}]}|});
  expect_failure ~name:"an array without items" ~message:{|cdp-gen: Demo.Thing: field "d" is an array without items|}
    (fun () -> parse_types {|{"id":"Thing","type":"object","properties":[{"name":"d","type":"array"}]}|});
  expect_failure ~name:"a non-boolean optional" ~message:{|cdp-gen: field "optional" must be a boolean, got "true"|}
    (fun () ->
    parse_types {|{"id":"Point","type":"object","properties":[{"name":"x","type":"integer","optional":"true"}]}|});
  expect_failure ~name:"an enum that is not a list" ~message:"cdp-gen: unhandled named type shape: Odd" (fun () ->
    parse_types {|{"id":"Odd","type":"string","enum":"x"}|});
  expect_failure ~name:"an unknown type name" ~message:{|cdp-gen: unhandled type in Demo: "date"|} (fun () ->
    parse_types {|{"id":"When","type":"date"}|});
  match parse_types {|{"type":"string"}|} with
  | exception Yojson.Safe.Util.Type_error (_message, _fragment) -> pass "a type without id is a shape error"
  | _parsed -> failwith "a type without id must not parse"

let () =
  let index = Cdp_gen.Model.Type_index.of_domains model_domains in
  (match Cdp_gen.Model.Type_index.find index { domain = "Network"; id = "RequestId" } with
  | Some { shape = Sealed_alias String; _ } -> ()
  | _other -> failwith "Network.RequestId is a sealed string");
  (match Cdp_gen.Model.Type_index.find index { domain = "Network"; id = "Nope" } with
  | None -> ()
  | Some _found -> failwith "an unknown type must not be found");
  pass "Type_index finds by domain and id"

let () =
  let picked = Cdp_gen.Model.Selection.parse "Page,Network" in
  assert (Cdp_gen.Model.Selection.contains picked "Page");
  assert (not (Cdp_gen.Model.Selection.contains picked "DOM"));
  assert (Cdp_gen.Model.Selection.contains (Cdp_gen.Model.Selection.parse "all") "DOM");
  pass "Selection"
