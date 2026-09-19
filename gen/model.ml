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

(* Stops a bad protocol name, where the message can name its owner.
   Without it the compiler would fail inside a generated file, far from the cause.
   Example: Page.Frame: name "x y" is not a plain identifier *)
module Identifier : sig
  val check_plain : owner:string -> string -> unit
  val check_enum_values : owner:string -> string list -> unit
end = struct
  let is_identifier_char ch = Protocol.is_letter ch || Protocol.is_digit ch || Char.equal ch '_'

  let refuse_name ~owner ~name = failwith (Printf.sprintf "cdp-gen: %s: name %S is not a plain identifier" owner name)

  let check_plain ~owner name =
    match name with
    | "" -> refuse_name ~owner ~name
    | _not_empty ->
    match Protocol.is_letter name.[0] && String.for_all is_identifier_char name with
    | true -> ()
    | false -> refuse_name ~owner ~name

  (* to avoid case when two values that become one constructor would give
  a variant with a duplicate case *)
  let check_enum_values ~owner values =
    let seen = Hashtbl.create 8 in
    let check value =
      match value with
      | "" -> failwith (Printf.sprintf "cdp-gen: %s: an enum value is empty" owner)
      | text ->
        let constructor = Naming.constructor_of_enum_value text in
        (match Hashtbl.find_opt seen constructor with
        | None -> Hashtbl.replace seen constructor text
        | Some earlier ->
          failwith
            (Printf.sprintf "cdp-gen: %s: enum values %S and %S both become the constructor %s" owner earlier text
               constructor))
    in
    List.iter check values
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

  let to_values = function
    | Scalar values | Array values -> values

  (* Enum values sit on the property or on its array items. *)
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

  let list_of_json ~domain ~owner field json = List.map (of_json ~domain ~owner) (Protocol.get_list field json)

  (* two fields that become one OCaml label would make the compiler reject the record *)
  let check_unique_labels ~owner fields =
    let seen = Hashtbl.create 8 in
    let check field =
      let label = Naming.sanitize_lower field.name in
      match Hashtbl.find_opt seen label with
      | None -> Hashtbl.replace seen label field.name
      | Some earlier ->
        failwith (Printf.sprintf "cdp-gen: %s: fields %S and %S both become %s" owner earlier field.name label)
    in
    List.iter check fields

  let check_identifiers ~owner fields =
    check_unique_labels ~owner fields;
    let check field =
      Identifier.check_plain ~owner field.name;
      match field.shape with
      | Enum inline_enum -> Identifier.check_enum_values ~owner (Inline_enum.to_values inline_enum)
      | Typed _type_expr -> ()
    in
    List.iter check fields
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

  (* An id type like RequestId is only a primitive with nothing else on it. It gets
     a sealed module, so a FrameId can never be passed where a RequestId is expected. *)
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

  let check_identifiers ~domain type_def =
    let owner = Printf.sprintf "%s.%s" domain type_def.id in
    Identifier.check_plain ~owner type_def.id;
    match type_def.shape with
    | Alias _type_expr -> ()
    | Sealed_alias _primitive -> ()
    | Record fields -> Property.check_identifiers ~owner fields
    | Enum values | Enum_list values -> Identifier.check_enum_values ~owner values
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

  let check_identifiers ~domain command =
    let owner = Printf.sprintf "%s.%s" domain command.name in
    Identifier.check_plain ~owner command.name;
    Property.check_identifiers ~owner command.params;
    Property.check_identifiers ~owner command.returns
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

  let check_identifiers ~domain event =
    let owner = Printf.sprintf "%s.%s" domain event.name in
    Identifier.check_plain ~owner event.name;
    Property.check_identifiers ~owner event.params
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

  let list_of_json json = Util.member "domains" json |> Util.to_list |> List.map of_json

  (* a name defined twice would silently overwrite its sibling in the output,
     or, for a command, hide behind the _command fallback module name *)
  let check_unique_names domains =
    let check_unique ~what names =
      let seen = Hashtbl.create 16 in
      let check name =
        match Hashtbl.mem seen name with
        | false -> Hashtbl.replace seen name ()
        | true -> failwith (Printf.sprintf "cdp-gen: %s %s is defined twice" what name)
      in
      List.iter check names
    in
    let check_unique_files domains =
      let seen = Hashtbl.create 16 in
      let check domain =
        let file = Naming.file_of_domain domain.name ^ ".ml" in
        match Hashtbl.find_opt seen file with
        | None -> Hashtbl.replace seen file domain.name
        | Some earlier -> failwith (Printf.sprintf "cdp-gen: domains %s and %s both become %s" earlier domain.name file)
      in
      List.iter check domains
    in
    check_unique ~what:"domain" (List.map (fun domain -> domain.name) domains);
    check_unique_files domains;
    let check_domain domain =
      let qualify name = Printf.sprintf "%s.%s" domain.name name in
      check_unique ~what:"type" (List.map (fun (type_def : Type_def.t) -> qualify type_def.id) domain.types);
      check_unique ~what:"command" (List.map (fun (command : Command.t) -> qualify command.name) domain.commands);
      check_unique ~what:"event" (List.map (fun (event : Event.t) -> qualify event.name) domain.events)
    in
    List.iter check_domain domains

  let check_identifiers domains =
    let check_domain domain =
      List.iter (Type_def.check_identifiers ~domain:domain.name) domain.types;
      List.iter (Command.check_identifiers ~domain:domain.name) domain.commands;
      List.iter (Event.check_identifiers ~domain:domain.name) domain.events
    in
    List.iter check_domain domains
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

(* One lookup for every step that follows a $ref, so a type can never be found
   in one step and missed in another. *)
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
