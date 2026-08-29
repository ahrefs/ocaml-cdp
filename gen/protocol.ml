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
  | `Null -> false
  | wrong_type -> failwith (spf "cdp-gen: field %S must be a boolean, got %s" field (Json.to_string wrong_type))

let is_letter ch = (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')
let is_digit ch = ch >= '0' && ch <= '9'

(* a domain name becomes output file names and module names in generated
   code, so anything beyond a plain identifier (a path separator, a comment
   opener) must be rejected before it reaches the filesystem *)
let checked_domain_name name =
  let plain =
    String.length name > 0 && is_letter name.[0] && String.for_all (fun ch -> is_letter ch || is_digit ch) name
  in
  if plain then name
  else failwith (spf "cdp-gen: domain name %S is not a plain identifier (letters and digits only)" name)

let load_domains path =
  let json = Json.from_file path in
  Util.member "domains" json
  |> Util.to_list
  |> List.map (fun domain_json ->
    {
      name = checked_domain_name (jstr "domain" domain_json);
      types = jlist "types" domain_json;
      commands = jlist "commands" domain_json;
      events = jlist "events" domain_json;
    })

(* a domain name or type id defined twice would silently overwrite its
   sibling in the output; refuse instead of generating wrong code *)
let check_unique_names domains =
  let seen_domains = Hashtbl.create 16 in
  List.iter
    (fun domain ->
      (match Hashtbl.mem seen_domains domain.name with
      | false -> Hashtbl.add seen_domains domain.name ()
      | true -> failwith (spf "cdp-gen: domain %s is defined twice" domain.name));
      let seen_types = Hashtbl.create 16 in
      List.iter
        (fun type_def ->
          let id = jstr "id" type_def in
          match Hashtbl.mem seen_types id with
          | false -> Hashtbl.add seen_types id ()
          | true -> failwith (spf "cdp-gen: type %s.%s is defined twice" domain.name id))
        domain.types)
    domains

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

(* a $ref to a type that exists nowhere would surface much later, as a
   compile error inside generated code; refuse with the referrer's name.
   Refs to types in loaded-but-unselected domains are left to the emitter's
   selection check, which names the missing domain. *)
let check_refs_exist ~all ~selected =
  let defined = Hashtbl.create 256 in
  List.iter
    (fun domain -> List.iter (fun type_def -> Hashtbl.add defined (domain.name, jstr "id" type_def) ()) domain.types)
    all;
  List.iter
    (fun domain ->
      let check_fragment ~owner fragment =
        List.iter
          (fun ref_string ->
            let target_domain, target_id = parse_ref ~current:domain.name ref_string in
            match Hashtbl.mem defined (target_domain, target_id) with
            | true -> ()
            | false -> failwith (spf "cdp-gen: %s references %s.%s, which does not exist" owner target_domain target_id))
          (collect_refs fragment [])
      in
      List.iter
        (fun type_def -> check_fragment ~owner:(spf "%s.%s" domain.name (jstr "id" type_def)) type_def)
        domain.types;
      List.iter
        (fun command -> check_fragment ~owner:(spf "%s.%s" domain.name (jstr "name" command)) command)
        domain.commands;
      List.iter (fun event -> check_fragment ~owner:(spf "%s.%s" domain.name (jstr "name" event)) event) domain.events)
    selected
