(* Model.Domain.of_json decides the kind of every type once.
   These pin the corner cases and the shape errors. *)

let pass name = Printf.printf "PASS %s\n" name

let expect_failure ~name ~message run =
  match run () with
  | exception Failure actual when String.equal actual message -> pass name
  | exception Failure actual -> failwith (Printf.sprintf "%s: wrong message %S" name actual)
  | _unexpected_success -> failwith ("expected a failure, got a result: " ^ name)

let protocol_files = [ "../../protocol/browser_protocol.json"; "../../protocol/js_protocol.json" ]

let real_domains =
  List.concat_map (fun path -> Yojson.Safe.from_file path |> Cdp_gen.Model.Domain.list_of_json) protocol_files

let () =
  Cdp_gen.Model.Domain.check_unique_names real_domains;
  Cdp_gen.Model.Domain.check_identifiers real_domains;
  let index = Cdp_gen.Model.Type_index.of_domains real_domains in
  (match Cdp_gen.Model.Type_index.find index { domain = "Network"; id = "RequestId" } with
  | Some { shape = Sealed_alias String; _ } -> ()
  | _other -> failwith "Network.RequestId is a sealed string");
  (match Cdp_gen.Model.Type_index.find index { domain = "Network"; id = "Nope" } with
  | None -> ()
  | Some _found -> failwith "an unknown type must not be found");
  pass (Printf.sprintf "the real protocol parses and checks: %d domains" (List.length real_domains))

let parse_types raw =
  let domain = Cdp_gen.Model.Domain.of_string (Printf.sprintf {|{"domain":"Demo","types":[%s]}|} raw) in
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
  (match (parse_only_type {|{"id":"Id","type":"string"}|}).shape with
  | Sealed_alias String -> ()
  | _other -> failwith "a bare primitive is a sealed alias");
  (match
     (parse_only_type {|{"id":"Point","type":"object","properties":[{"name":"x","type":"integer","optional":true}]}|})
       .shape
   with
  | Record [ { name = "x"; shape = Typed (Primitive Integer); optional = true; _ } ] -> ()
  | _other -> failwith "a record field");
  pass "type-def kinds on hand-written shapes"

let () =
  let type_def =
    parse_only_type
      {|{"id":"Thing","type":"object","properties":[
          {"name":"other","$ref":"Other.Id"},
          {"name":"points","type":"array","items":{"$ref":"Point"}},
          {"name":"mode","type":"string","enum":["a"]}]}|}
  in
  (match Cdp_gen.Model.Type_def.collect_refs type_def with
  | [ { domain = "Other"; id = "Id" }; { domain = "Demo"; id = "Point" } ] -> ()
  | _other -> failwith "collect_refs finds refs in fields and array items, in order");
  pass "collect_refs"

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
  expect_failure ~name:"a domain name with a slash"
    ~message:{|cdp-gen: domain name "De/mo" is not a plain identifier (letters and digits only)|} (fun () ->
    Cdp_gen.Model.Domain.of_string {|{"domain":"De/mo"}|});
  match parse_types {|{"type":"string"}|} with
  | exception Yojson.Safe.Util.Type_error (_message, _fragment) -> pass "a type without id is a shape error"
  | _parsed -> failwith "a type without id must not parse"

let () =
  let picked = Cdp_gen.Model.Selection.parse "Page,Network" in
  assert (Cdp_gen.Model.Selection.contains picked "Page");
  assert (not (Cdp_gen.Model.Selection.contains picked "DOM"));
  assert (Cdp_gen.Model.Selection.contains (Cdp_gen.Model.Selection.parse "all") "DOM");
  pass "Selection"
