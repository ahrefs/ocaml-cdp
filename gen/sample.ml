(* One sample JSON value per protocol type, following the schema.
   - every field gets a sample value; an optional one is omitted when that would not end
   - an array gets one element, or [] when the element type is being built already
   - re-entering a type through a required non-array field cannot end. it fails and the caller skips the type *)

let of_enum_values values : Yojson.Safe.t =
  match values with
  | first :: _rest -> `String first
  | [] -> `String "sample"

let is_visiting ~visiting type_ref = List.exists (Model.Type_ref.equal type_ref) visiting

let rec of_type_expr ~type_index ~visiting (type_expr : Model.Type_expr.t) : Yojson.Safe.t =
  match type_expr with
  | Primitive String -> `String "sample"
  | Primitive Integer -> `Int 7
  | Primitive Number -> `Float 1.5
  | Primitive Boolean -> `Bool true
  | Binary -> `String "c2FtcGxl"
  | Any -> `Assoc []
  | Array (Ref type_ref) when is_visiting ~visiting type_ref -> `List []
  | Array items -> `List [ of_type_expr ~type_index ~visiting items ]
  | Ref type_ref -> of_named ~type_index ~visiting type_ref

and of_named ~type_index ~visiting (type_ref : Model.Type_ref.t) : Yojson.Safe.t =
  (match is_visiting ~visiting type_ref with
  | true -> failwith (Printf.sprintf "cdp-gen: required-field cycle through %s.%s" type_ref.domain type_ref.id)
  | false -> ());
  match Model.Type_index.find type_index type_ref with
  | None -> failwith (Printf.sprintf "cdp-gen: sample synthesis: unknown type %s.%s" type_ref.domain type_ref.id)
  | Some type_def -> of_type_def ~type_index ~visiting:(type_ref :: visiting) type_def

and of_type_def ~type_index ~visiting (type_def : Model.Type_def.t) : Yojson.Safe.t =
  match type_def.shape with
  | Sealed_alias primitive -> of_type_expr ~type_index ~visiting (Primitive primitive)
  | Enum values -> of_enum_values values
  | Enum_list values -> `List [ of_enum_values values ]
  | Record fields -> of_properties ~type_index ~visiting fields
  | Alias type_expr -> of_type_expr ~type_index ~visiting type_expr

and of_properties ~type_index ~visiting fields : Yojson.Safe.t =
  `Assoc (List.filter_map (of_property ~type_index ~visiting) fields)

(* An optional field is filled too, unless that cannot end:
   - its type is being built already (recursion through the option)
   - its type has no finite sample of its own
   A required field in that position fails loudly instead. *)
and of_property ~type_index ~visiting (property : Model.Property.t) =
  let build_value () =
    match property.shape with
    | Enum (Scalar values) -> of_enum_values values
    | Enum (Array values) -> `List [ of_enum_values values ]
    | Typed type_expr -> of_type_expr ~type_index ~visiting type_expr
  in
  match property.optional with
  | false -> Some (property.name, build_value ())
  | true ->
  match build_value () with
  | value -> Some (property.name, value)
  | exception Failure _no_finite_sample -> None
