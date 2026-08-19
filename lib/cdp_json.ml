(* Hand-written glue between generated code and the melange-json runtime.
   This is the only file that knows which JSON backend is in use
   (native: Yojson.Basic.t via melange-json-native). *)

(** A raw JSON value. Used for protocol fields typed "any" or bare "object". *)
type t = Melange_json.t

(* identity codecs: lets [@@deriving json] work on fields typed [Cdp_json.t] *)
let of_json (json : Melange_json.t) : t = json
let to_json (value : t) : Melange_json.t = value

(* derive layer for [Cdp_json.t] fields: [@@deriving show, eq] looks these up *)
let equal : t -> t -> bool = Yojson.Basic.equal
let show (value : t) : string = Yojson.Basic.to_string value
let pp fmt (value : t) = Format.pp_print_string fmt (show value)

(** Payload of catch-all [Other] constructors: an enum value this protocol revision does not know. [tag] is the raw wire
    string. Same type as [Melange_json.unknown_variant_case], re-exported under a name the derive layer can find helpers
    for. *)
type unknown = Melange_json.unknown_variant_case = {
  tag : string;
  payload : t list option;
}

let equal_unknown (left : unknown) (right : unknown) : bool =
  String.equal left.tag right.tag
  &&
  match left.payload, right.payload with
  | None, None -> true
  | Some left_items, Some right_items ->
    (try List.for_all2 equal left_items right_items with Invalid_argument _uneven_lengths -> false)
  | None, Some _ | Some _, None -> false

let show_unknown (case : unknown) : string = Printf.sprintf "Unknown %S" case.tag
let pp_unknown fmt (case : unknown) = Format.pp_print_string fmt (show_unknown case)
