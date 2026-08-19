(* Unit tests for Emit.map_type: protocol type description -> OCaml type
   expression. *)

let pass name = Printf.printf "PASS %s\n" name

let expect_failure name run =
  match run () with
  | exception Failure _message -> pass name
  | _unexpected_success -> failwith ("expected a failure, got a result: " ^ name)

(* setup: two selected domains; one sealed alias in each *)
let selected = [ "Demo"; "Other" ]

let alias_tbl =
  let tbl = Hashtbl.create 4 in
  Hashtbl.replace tbl ("Demo", "LocalAlias") "number";
  Hashtbl.replace tbl ("Other", "AliasId") "string";
  tbl

let map raw = Cdp_gen.Emit.map_type ~selected ~alias_tbl ~domain:"Demo" (Yojson.Safe.from_string raw)

let () =
  assert (map {|{"type":"string"}|} = "string");
  assert (map {|{"type":"integer"}|} = "int");
  assert (map {|{"type":"number"}|} = "float");
  assert (map {|{"type":"boolean"}|} = "bool");
  assert (map {|{"type":"binary"}|} = "string");
  pass "primitives"

let () =
  assert (map {|{"type":"any"}|} = "Cdp_json.t");
  assert (map {|{"type":"object"}|} = "Cdp_json.t");
  pass "any and bare object become raw JSON"

let () =
  assert (map {|{"type":"array","items":{"type":"integer"}}|} = "int list");
  assert (map {|{"type":"array","items":{"type":"array","items":{"type":"string"}}}|} = "string list list");
  pass "arrays, including nested"

let () =
  assert (map {|{"$ref":"Widget"}|} = "widget");
  assert (map {|{"$ref":"Other.Thing"}|} = "Cdp_other_types.thing");
  assert (map {|{"type":"array","items":{"$ref":"Widget"}}|} = "widget list");
  pass "named refs, same and cross domain"

let () =
  assert (map {|{"$ref":"LocalAlias"}|} = "Cdp_base.Demo.Local_alias.t");
  assert (map {|{"$ref":"Other.AliasId"}|} = "Cdp_base.Other.Alias_id.t");
  pass "sealed aliases route through Cdp_base"

let () =
  expect_failure "ref outside the selected set fails" (fun () -> map {|{"$ref":"Unknown.Thing"}|});
  expect_failure "unknown primitive fails" (fun () -> map {|{"type":"weird"}|})

let () = print_endline "all emit tests passed"
