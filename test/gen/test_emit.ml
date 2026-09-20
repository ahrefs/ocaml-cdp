(* Unit tests for Emit.render_type_expr: protocol type description -> OCaml type
   expression. *)

let pass name = Printf.printf "PASS %s\n" name

let expect_failure ~name run =
  match run () with
  | exception Failure _message -> pass name
  | _unexpected_success -> failwith ("expected a failure, got a result: " ^ name)

(* setup: two domains; one sealed alias in each *)

let type_index =
  Cdp_gen.Model.Type_index.of_domains
    [
      Cdp_gen.Model.Domain.of_string {|{"domain":"Demo","types":[{"id":"LocalAlias","type":"number"}]}|};
      Cdp_gen.Model.Domain.of_string {|{"domain":"Other","types":[{"id":"AliasId","type":"string"}]}|};
    ]

let render raw =
  let type_expr = Cdp_gen.Model.Type_expr.of_json ~domain:"Demo" (Yojson.Safe.from_string raw) in
  Cdp_gen.Emit.render_type_expr ~type_index ~domain:"Demo" type_expr

let () =
  assert (String.equal (render {|{"type":"string"}|}) "string");
  assert (String.equal (render {|{"type":"integer"}|}) "int");
  assert (String.equal (render {|{"type":"number"}|}) "Cdp_json.number");
  assert (String.equal (render {|{"type":"boolean"}|}) "bool");
  assert (String.equal (render {|{"type":"binary"}|}) "string");
  pass "primitives"

let () =
  assert (String.equal (render {|{"type":"any"}|}) "Cdp_json.t");
  assert (String.equal (render {|{"type":"object"}|}) "Cdp_json.t");
  pass "any and bare object become raw JSON"

let () =
  assert (String.equal (render {|{"type":"array","items":{"type":"integer"}}|}) "int list");
  assert (
    String.equal (render {|{"type":"array","items":{"type":"array","items":{"type":"string"}}}|}) "string list list");
  pass "arrays, including nested"

let () =
  assert (String.equal (render {|{"$ref":"Widget"}|}) "widget");
  assert (String.equal (render {|{"$ref":"Other.Thing"}|}) "Cdp_other_types.thing");
  assert (String.equal (render {|{"type":"array","items":{"$ref":"Widget"}}|}) "widget list");
  pass "named refs, same and cross domain"

let () =
  assert (String.equal (render {|{"$ref":"LocalAlias"}|}) "Cdp_base.Demo.Local_alias.t");
  assert (String.equal (render {|{"$ref":"Other.AliasId"}|}) "Cdp_base.Other.Alias_id.t");
  pass "sealed aliases route through Cdp_base"

let () = expect_failure ~name:"unknown primitive fails" (fun () -> render {|{"type":"weird"}|})

let () = print_endline "all emit tests passed"
