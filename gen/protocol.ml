(* Reading the protocol JSON: the domain model, reference collection,
   the primitive-alias table, and the cross-domain cycle check. *)

module Json = Yojson.Safe
module Util = Yojson.Safe.Util

let spf = Printf.sprintf

type domain = {
  name : string;
  types : Json.t list;
  commands : Json.t list;
  events : Json.t list;
}

let jstr field json = Util.member field json |> Util.to_string
let jlist field json =
  match Util.member field json with
  | `Null -> []
  | value -> Util.to_list value
let jbool field json =
  match Util.member field json with
  | `Bool value -> value
  | _not_a_bool -> false

let load_domains path =
  let json = Json.from_file path in
  Util.member "domains" json
  |> Util.to_list
  |> List.map (fun domain_json ->
    {
      name = jstr "domain" domain_json;
      types = jlist "types" domain_json;
      commands = jlist "commands" domain_json;
      events = jlist "events" domain_json;
    })

let read_file path =
  let input = open_in path in
  let length = in_channel_length input in
  let contents = really_input_string input length in
  close_in input;
  String.trim contents

let write_file path contents =
  let output = open_out path in
  output_string output contents;
  close_out output

let parse_ref ~current ref_string =
  match String.index_opt ref_string '.' with
  | None -> current, ref_string
  | Some dot -> String.sub ref_string 0 dot, String.sub ref_string (dot + 1) (String.length ref_string - dot - 1)

(* every "$ref" string reachable inside a json fragment *)
let rec collect_refs (json : Json.t) acc =
  match json with
  | `List items -> List.fold_left (fun acc item -> collect_refs item acc) acc items
  | `Assoc fields ->
    List.fold_left
      (fun acc (key, value) ->
        match key, value with
        | "$ref", `String ref_string -> ref_string :: acc
        | _not_a_ref -> collect_refs value acc)
      acc fields
  | _scalar -> acc

let is_primitive_alias type_def =
  let has field = Util.member field type_def <> `Null in
  if has "enum" || has "properties" || has "items" then None
  else (
    match Util.member "type" type_def with
    | `String (("string" | "integer" | "number" | "boolean") as prim) -> Some prim
    | _not_a_primitive -> None)

(* (domain, TypeId) -> primitive kind, for aliases like Network.RequestId *)
let build_alias_table domains =
  let table = Hashtbl.create 64 in
  List.iter
    (fun domain ->
      List.iter
        (fun type_def ->
          match is_primitive_alias type_def with
          | Some prim -> Hashtbl.replace table (domain.name, jstr "id" type_def) prim
          | None -> ())
        domain.types)
    domains;
  table

(* type-level cross-domain graph; edges to sealed aliases excluded because
   those references are routed through Base (a leaf). Must be a DAG. *)
let check_types_dag domains ~alias_tbl =
  let edges (domain : domain) =
    List.concat_map (fun type_def -> collect_refs type_def []) domain.types
    |> List.filter_map (fun ref_string ->
      let dom, id = parse_ref ~current:domain.name ref_string in
      match dom = domain.name with
      | true -> None
      | false -> if Hashtbl.mem alias_tbl (dom, id) then None (* routed through Base *) else Some dom)
    |> List.sort_uniq String.compare
  in
  let graph = List.map (fun (domain : domain) -> domain.name, edges domain) domains in
  let visiting = Hashtbl.create 16
  and finished = Hashtbl.create 16 in
  let rec visit path node =
    if Hashtbl.mem finished node then ()
    else if Hashtbl.mem visiting node then
      failwith
        (spf
           "cdp-gen: type-level cycle between domains: %s. Sealed-alias routing through Base cannot break it; the \
            domains in the cycle must be merged into one compilation unit."
           (String.concat " -> " (List.rev (node :: path))))
    else begin
      Hashtbl.add visiting node ();
      List.iter (visit (node :: path)) (try List.assoc node graph with Not_found -> []);
      Hashtbl.remove visiting node;
      Hashtbl.add finished node ()
    end
  in
  List.iter (fun (domain : domain) -> visit [] domain.name) domains
