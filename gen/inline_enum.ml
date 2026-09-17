(* An enum written inline on a property instead of as a named type.
   - the emitter hoists it to a named type
   - the roundtrip test checks every value of it *)

type t =
  | Scalar of string list
  | Array of string list

let of_prop prop =
  match Yojson.Safe.Util.member "enum" prop with
  | `List values -> Some (Scalar (List.map Yojson.Safe.Util.to_string values))
  | _no_scalar_enum ->
  match Yojson.Safe.Util.member "type" prop, Yojson.Safe.Util.member "items" prop with
  | `String "array", (`Assoc _ as items) ->
    (match Yojson.Safe.Util.member "enum" items with
    | `List values -> Some (Array (List.map Yojson.Safe.Util.to_string values))
    | _no_item_enum -> None)
  | _not_an_array -> None

let values = function
  | Scalar values | Array values -> values

let field_type inline_enum ~enum_name =
  match inline_enum with
  | Scalar _ -> enum_name
  | Array _ -> Printf.sprintf "%s list" enum_name
