(* Every step after parsing works on these values, never on JSON.
   - a wrong field name fails to compile instead of failing at run time
   - what kind of type something is gets decided once
   - each type reads itself from JSON, so a new field is added in one place
   - a shape the generator cannot print stops in of_json, with the owner's name *)

module Json = Yojson.Safe
module Util = Yojson.Safe.Util

let description_of_json json = Util.member "description" json |> Util.to_string_option

module Flags = struct
  type t = {
    deprecated : bool;
    experimental : bool;
    (* Chrome moved the command to another domain; the old name still works.
       Page.deleteCookie has redirect Network: use Network.deleteCookies instead.
       Printed as [@@alert redirected ...] so a caller of the old name gets a warning. *)
    redirect : string option;
  }

  let of_json json =
    {
      deprecated = Protocol.get_bool "deprecated" json;
      experimental = Protocol.get_bool "experimental" json;
      redirect = Util.member "redirect" json |> Util.to_string_option;
    }
end

module Primitive = struct
  type t =
    | String
    | Integer
    | Number
    | Boolean

  let of_name name =
    match name with
    | "string" -> Some String
    | "integer" -> Some Integer
    | "number" -> Some Number
    | "boolean" -> Some Boolean
    | _not_a_primitive -> None
end

module Type_ref = struct
  type t = {
    domain : string;
    id : string;
  }

  (* "Network.LoaderId" names its domain; "Frame" means the current one *)
  let parse ~current_domain ref_string =
    match String.index_opt ref_string '.' with
    | None -> { domain = current_domain; id = ref_string }
    | Some dot ->
      {
        domain = String.sub ref_string 0 dot;
        id = String.sub ref_string (dot + 1) (String.length ref_string - dot - 1);
      }
end

module Type_expr = struct
  type t =
    | Primitive of Primitive.t
    | Binary (* base64 text on the wire *)
    | Any (* "any", or "object" without properties *)
    | Ref of Type_ref.t
    | Array of t

  let rec of_json ~domain json =
    match Util.member "$ref" json with
    | `String ref_string -> Ref (Type_ref.parse ~current_domain:domain ref_string)
    | `Null ->
      (match Util.member "type" json with
      | `String "binary" -> Binary
      | `String ("any" | "object") -> Any
      | `String "array" -> Array (of_json ~domain (Util.member "items" json))
      | `String name as unexpected ->
        (match Primitive.of_name name with
        | Some primitive -> Primitive primitive
        | None -> failwith (Printf.sprintf "cdp-gen: unhandled type in %s: %s" domain (Json.to_string unexpected)))
      | unexpected -> failwith (Printf.sprintf "cdp-gen: unhandled type in %s: %s" domain (Json.to_string unexpected)))
    | unexpected -> failwith (Printf.sprintf "cdp-gen: bad $ref: %s" (Json.to_string unexpected))
end

module Inline_enum = struct
  type t =
    | Scalar of string list
    | Array of string list

  (* an enum written on the property itself, or on the items of its array *)
  let of_json json =
    match Util.member "enum" json with
    | `List values -> Some (Scalar (List.map Util.to_string values))
    | _no_scalar_enum ->
    match Util.member "type" json, Util.member "items" json with
    | `String "array", (`Assoc _ as items) ->
      (match Util.member "enum" items with
      | `List values -> Some (Array (List.map Util.to_string values))
      | _no_item_enum -> None)
    | _not_an_array -> None
end

module Property = struct
  type shape =
    | Typed of Type_expr.t
    | Enum of Inline_enum.t (* gets its own named type when printed *)

  type t = {
    name : string;
    shape : shape;
    optional : bool;
    description : string option;
    flags : Flags.t;
  }

  (* a property says one thing: a $ref, or a type. Two at once, or none, is
     refused here instead of being resolved by an unwritten precedence *)
  let check_shape ~owner ~field json =
    let has key = Protocol.has_field key json in
    let refuse what = failwith (Printf.sprintf "cdp-gen: %s: field %S %s" owner field what) in
    match has "$ref" with
    | true ->
      (match has "type", has "enum" with
      | false, false -> ()
      | false, true -> refuse "has both $ref and enum"
      | true, _any_enum -> refuse "has both $ref and type")
    | false ->
    match has "type" with
    | false -> refuse "has neither type nor $ref"
    | true ->
    match Util.member "type" json, has "items" with
    | `String "array", false -> refuse "is an array without items"
    | _typed -> ()

  let of_json ~domain ~owner json =
    let name = Protocol.get_string "name" json in
    check_shape ~owner ~field:name json;
    let shape =
      match Inline_enum.of_json json with
      | Some inline_enum -> Enum inline_enum
      | None -> Typed (Type_expr.of_json ~domain json)
    in
    {
      name;
      shape;
      optional = Protocol.get_bool "optional" json;
      description = description_of_json json;
      flags = Flags.of_json json;
    }

  (* the "properties", "parameters" or "returns" list of the owner *)
  let list_of_json ~domain ~owner field json = List.map (of_json ~domain ~owner) (Protocol.get_list field json)
end

module Type_def = struct
  type shape =
    | Sealed_alias of Primitive.t (* an id like RequestId: sealed in Cdp_base *)
    | Enum of string list
    | Enum_list of string list (* an array whose items carry the enum *)
    | Record of Property.t list (* never empty: [] is read as Alias Any *)
    | Alias of Type_expr.t

  type t = {
    id : string;
    shape : shape;
    description : string option;
    flags : Flags.t;
  }

  (* a type that is only a primitive, with nothing else on it, is an id: RequestId, FrameId *)
  let sealed_primitive_of_json json =
    let has key = Protocol.has_field key json in
    match has "enum" || has "properties" || has "items" with
    | true -> None
    | false ->
    match Util.member "type" json with
    | `String name -> Primitive.of_name name
    | _not_a_primitive -> None

  let of_json ~domain json =
    let id = Protocol.get_string "id" json in
    let owner = Printf.sprintf "%s.%s" domain id in
    let shape =
      match Util.member "enum" json, Util.member "properties" json with
      | `List values, _any_properties -> Enum (List.map Util.to_string values)
      | `Null, `List (_ :: _) -> Record (Property.list_of_json ~domain ~owner "properties" json)
      | `Null, _no_fields ->
        (match Inline_enum.of_json json, sealed_primitive_of_json json with
        | Some (Array values), _ -> Enum_list values
        | _no_item_enum, Some primitive -> Sealed_alias primitive
        | _no_item_enum, None -> Alias (Type_expr.of_json ~domain json))
      | _unexpected_shape -> failwith ("cdp-gen: unhandled named type shape: " ^ id)
    in
    { id; shape; description = description_of_json json; flags = Flags.of_json json }
end

module Command = struct
  type t = {
    name : string;
    params : Property.t list;
    returns : Property.t list;
    description : string option;
    flags : Flags.t;
  }

  let of_json ~domain json =
    let name = Protocol.get_string "name" json in
    let owner = Printf.sprintf "%s.%s" domain name in
    {
      name;
      params = Property.list_of_json ~domain ~owner "parameters" json;
      returns = Property.list_of_json ~domain ~owner "returns" json;
      description = description_of_json json;
      flags = Flags.of_json json;
    }
end

module Event = struct
  type t = {
    name : string;
    params : Property.t list;
    description : string option;
    flags : Flags.t;
  }

  let of_json ~domain json =
    let name = Protocol.get_string "name" json in
    let owner = Printf.sprintf "%s.%s" domain name in
    {
      name;
      params = Property.list_of_json ~domain ~owner "parameters" json;
      description = description_of_json json;
      flags = Flags.of_json json;
    }
end

module Domain = struct
  type t = {
    name : string;
    types : Type_def.t list;
    commands : Command.t list;
    events : Event.t list;
    description : string option;
    flags : Flags.t;
  }

  let of_json json =
    let name = Protocol.check_domain_name (Protocol.get_string "domain" json) in
    {
      name;
      types = List.map (Type_def.of_json ~domain:name) (Protocol.get_list "types" json);
      commands = List.map (Command.of_json ~domain:name) (Protocol.get_list "commands" json);
      events = List.map (Event.of_json ~domain:name) (Protocol.get_list "events" json);
      description = description_of_json json;
      flags = Flags.of_json json;
    }

  (* the "domains" of one protocol file *)
  let list_of_json json = Util.member "domains" json |> Util.to_list |> List.map of_json
end

module Selection = struct
  type t =
    | All
    | Named of string list

  let parse argument =
    match argument with
    | "all" -> All
    | names -> Named (String.split_on_char ',' names)

  let contains selection domain_name =
    match selection with
    | All -> true
    | Named names -> List.mem domain_name names
end

(* every named type of the loaded protocol, found by domain and id *)
module Type_index : sig
  type t

  val of_domains : Domain.t list -> t
  val find : t -> Type_ref.t -> Type_def.t option
end = struct
  type t = (Type_ref.t, Type_def.t) Hashtbl.t

  let of_domains domains =
    let index = Hashtbl.create 256 in
    let add_type ~domain_name (type_def : Type_def.t) =
      let key = { Type_ref.domain = domain_name; id = type_def.id } in
      Hashtbl.replace index key type_def
    in
    let add_domain (domain : Domain.t) = List.iter (add_type ~domain_name:domain.name) domain.types in
    List.iter add_domain domains;
    index

  let find index type_ref = Hashtbl.find_opt index type_ref
end

module Item = struct
  type kind =
    | Command of Command.t
    | Event of Event.t

  type t = {
    module_name : string; (* Capture_screenshot, or Navigate_command after a clash *)
    wire_name : string; (* Page.captureScreenshot *)
    kind : kind;
  }
end

module Decl = struct
  type t = {
    name : string;
    body : string;
  }
end

module Output_file = struct
  type t = {
    name : string;
    contents : string;
  }
end
