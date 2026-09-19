(* Every step after parsing works on these values, never on JSON.
   - a wrong field name fails to compile instead of failing at run time
   - what kind of type something is gets decided once
   - each type reads itself from JSON, so a new field is added in one place
   - a shape the generator cannot print stops in of_json, with the owner's name *)

module Json = Yojson.Safe
module Util = Yojson.Safe.Util

module Json_field = struct
  let read_string field json = Util.member field json |> Util.to_string

  let read_list field json =
    match Util.member field json with
    | `Null -> []
    | value -> Util.to_list value

  let has field json =
    match Util.member field json with
    | `Null -> false
    | _present -> true

  (* a missing flag means false; a non-boolean would silently flip a field to required *)
  let read_bool field json =
    match Util.member field json with
    | `Null -> false
    | `Bool value -> value
    | wrong_type ->
      failwith (Printf.sprintf "cdp-gen: field %S must be a boolean, got %s" field (Json.to_string wrong_type))
end

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
      deprecated = Json_field.read_bool "deprecated" json;
      experimental = Json_field.read_bool "experimental" json;
      redirect = Util.member "redirect" json |> Util.to_string_option;
    }

  type alert = {
    attribute : string; (* ocaml.deprecated, alert redirected, alert experimental *)
    message : string;
  }

  let to_alerts { deprecated; experimental; redirect } =
    let deprecated_alert =
      match deprecated, redirect with
      | false, None -> []
      | true, None -> [ { attribute = "ocaml.deprecated"; message = "deprecated in CDP" } ]
      | true, Some domain ->
        let message = Printf.sprintf "deprecated in CDP, redirected to the %s domain" domain in
        [ { attribute = "ocaml.deprecated"; message } ]
      | false, Some domain ->
        let message = Printf.sprintf "redirected to the %s domain in CDP" domain in
        [ { attribute = "alert redirected"; message } ]
    in
    let experimental_alert =
      match experimental with
      | false -> []
      | true -> [ { attribute = "alert experimental"; message = "experimental in CDP, may change with Chrome" } ]
    in
    deprecated_alert @ experimental_alert
end

module Primitive = struct
  type t =
    | String
    | Integer
    | Number
    | Boolean

  let to_ocaml_type = function
    | String -> "string"
    | Integer -> "int"
    | Number -> "Cdp_json.number"
    | Boolean -> "bool"

  let of_name name =
    match name with
    | "string" -> Some String
    | "integer" -> Some Integer
    | "number" -> Some Number
    | "boolean" -> Some Boolean
    | _not_a_primitive -> None

  type sealed_module = {
    underlying_type : string; (* float for Number: the sealed type hides the alias *)
    conversion : string; (* of_<conversion> / to_<conversion> *)
    equal_module : string;
    codec : string;
    show_expression : string;
  }

  let to_sealed_module = function
    | String ->
      {
        underlying_type = "string";
        conversion = "string";
        equal_module = "String";
        codec = "Jsonkit.Primitives.string";
        show_expression = "Printf.sprintf \"%S\" value";
      }
    | Integer ->
      {
        underlying_type = "int";
        conversion = "int";
        equal_module = "Int";
        codec = "Jsonkit.Primitives.int";
        show_expression = "string_of_int value";
      }
    | Number ->
      {
        underlying_type = "float";
        conversion = "float";
        equal_module = "Float";
        codec = "Cdp_json.number";
        show_expression = "string_of_float value";
      }
    | Boolean ->
      {
        underlying_type = "bool";
        conversion = "bool";
        equal_module = "Bool";
        codec = "Jsonkit.Primitives.bool";
        show_expression = "string_of_bool value";
      }
end

module Type_ref = struct
  type t = {
    domain : string;
    id : string;
  }

  let equal a b = String.equal a.domain b.domain && String.equal a.id b.id

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
  let is_identifier_char ch = Naming.is_letter ch || Naming.is_digit ch || Char.equal ch '_'

  let refuse_name ~owner ~name = failwith (Printf.sprintf "cdp-gen: %s: name %S is not a plain identifier" owner name)

  let check_plain ~owner name =
    match name with
    | "" -> refuse_name ~owner ~name
    | _not_empty ->
    match Naming.is_letter name.[0] && String.for_all is_identifier_char name with
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

  let rec collect_refs type_expr =
    match type_expr with
    | Ref type_ref -> [ type_ref ]
    | Array items -> collect_refs items
    | Primitive _ | Binary | Any -> []
end

module Inline_enum = struct
  type t =
    | Scalar of string list
    | Array of string list

  let to_values = function
    | Scalar values | Array values -> values

  let to_ocaml_type inline_enum ~enum_name =
    match inline_enum with
    | Scalar _values -> enum_name
    | Array _values -> Printf.sprintf "%s list" enum_name

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
    let has key = Json_field.has key json in
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
    let name = Json_field.read_string "name" json in
    check_shape ~owner ~field:name json;
    let shape =
      match Inline_enum.of_json json with
      | Some inline_enum -> Enum inline_enum
      | None -> Typed (Type_expr.of_json ~domain json)
    in
    {
      name;
      shape;
      optional = Json_field.read_bool "optional" json;
      description = description_of_json json;
      flags = Flags.of_json json;
    }

  let list_of_json ~domain ~owner field json = List.map (of_json ~domain ~owner) (Json_field.read_list field json)

  let collect_refs property =
    match property.shape with
    | Enum _ -> []
    | Typed type_expr -> Type_expr.collect_refs type_expr

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
      | Typed _type_expr -> ()
      | Enum inline_enum -> Identifier.check_enum_values ~owner (Inline_enum.to_values inline_enum)
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
    let has key = Json_field.has key json in
    match has "enum" || has "properties" || has "items" with
    | true -> None
    | false ->
    match Util.member "type" json with
    | `String name -> Primitive.of_name name
    | _not_a_primitive -> None

  let of_json ~domain json =
    let id = Json_field.read_string "id" json in
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

  let sealed_primitive_of type_def =
    match type_def.shape with
    | Sealed_alias primitive -> Some primitive
    | Enum _ | Enum_list _ | Record _ | Alias _ -> None

  let collect_refs type_def =
    match type_def.shape with
    | Sealed_alias _ | Enum _ | Enum_list _ -> []
    | Alias type_expr -> Type_expr.collect_refs type_expr
    | Record fields -> List.concat_map Property.collect_refs fields

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
    let name = Json_field.read_string "name" json in
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

  let collect_refs command = List.concat_map Property.collect_refs (command.params @ command.returns)
end

module Event = struct
  type t = {
    name : string;
    params : Property.t list;
    description : string option;
    flags : Flags.t;
  }

  let of_json ~domain json =
    let name = Json_field.read_string "name" json in
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

  let collect_refs event = List.concat_map Property.collect_refs event.params
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

  (* a domain name becomes file and module names, so a path separator or a
     comment opener must be refused before it reaches the filesystem *)
  let check_name name =
    let is_plain_char ch = Naming.is_letter ch || Naming.is_digit ch in
    let is_plain =
      match name with
      | "" -> false
      | _not_empty -> Naming.is_letter name.[0] && String.for_all is_plain_char name
    in
    match is_plain with
    | true -> name
    | false ->
      failwith (Printf.sprintf "cdp-gen: domain name %S is not a plain identifier (letters and digits only)" name)

  let of_json json =
    let name = check_name (Json_field.read_string "domain" json) in
    {
      name;
      types = List.map (Type_def.of_json ~domain:name) (Json_field.read_list "types" json);
      commands = List.map (Command.of_json ~domain:name) (Json_field.read_list "commands" json);
      events = List.map (Event.of_json ~domain:name) (Json_field.read_list "events" json);
      description = description_of_json json;
      flags = Flags.of_json json;
    }

  let list_of_json json = Util.member "domains" json |> Util.to_list |> List.map of_json

  let of_string raw = raw |> Yojson.Safe.from_string |> of_json

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

  let collect_refs domain =
    List.concat_map Type_def.collect_refs domain.types
    @ List.concat_map Command.collect_refs domain.commands
    @ List.concat_map Event.collect_refs domain.events

  let count_sealed_aliases domains =
    let sealed_of_domain domain = List.filter_map Type_def.sealed_primitive_of domain.types in
    let sealed_of_all_domains = List.concat_map sealed_of_domain domains in
    List.length sealed_of_all_domains
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

  let description_of item =
    match item.kind with
    | Command command -> command.description
    | Event event -> event.description

  let flags_of item =
    match item.kind with
    | Command command -> command.flags
    | Event event -> event.flags

  (* A command module sits next to the re-exported sealed alias modules, so a
     command named like an alias type would clash: it steps aside as
     <name>_command, an event as <name>_event. No such pair exists in r1698617. *)
  let of_domain (domain : Domain.t) =
    let sealed_module_name (type_def : Type_def.t) =
      Option.map (fun _primitive -> Naming.submodule_of_name type_def.id) (Type_def.sealed_primitive_of type_def)
    in
    let used = ref (List.filter_map sealed_module_name domain.types) in
    let claim ~fallback_suffix proposed =
      let name =
        match List.mem proposed !used with
        | false -> proposed
        | true -> proposed ^ fallback_suffix
      in
      (match List.mem name !used with
      | false -> ()
      | true -> failwith (Printf.sprintf "cdp-gen: module name collision in %s: %s" domain.name name));
      used := name :: !used;
      name
    in
    let wire_name name = Printf.sprintf "%s.%s" domain.name name in
    let of_command (command : Command.t) =
      let module_name = claim ~fallback_suffix:"_command" (Naming.submodule_of_name command.name) in
      { module_name; wire_name = wire_name command.name; kind = Command command }
    in
    let of_event (event : Event.t) =
      let module_name = claim ~fallback_suffix:"_event" (Naming.submodule_of_name event.name) in
      { module_name; wire_name = wire_name event.name; kind = Event event }
    in
    let commands = List.map of_command domain.commands in
    let events = List.map of_event domain.events in
    commands @ events
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
