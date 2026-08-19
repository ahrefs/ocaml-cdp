(* Hand-written glue between generated code and the melange-json runtime.
   This is the only file that knows which JSON backend is in use
   (native: Yojson.Basic.t via melange-json-native). *)

(** A raw JSON value. Used for protocol fields typed "any" or bare "object". *)
type t = Melange_json.t

(* identity codecs: lets [@@deriving json] work on fields typed [Cdp_json.t] *)
let of_json (j : Melange_json.t) : t = j
let to_json (x : t) : Melange_json.t = x

(* derive layer for [Cdp_json.t] fields: [@@deriving show, eq] looks these up *)
let equal : t -> t -> bool = Yojson.Basic.equal
let show (x : t) : string = Yojson.Basic.to_string x
let pp fmt (x : t) = Format.pp_print_string fmt (show x)

(** Payload of catch-all [Other] constructors: an enum value this protocol revision does not know. [tag] is the raw wire
    string. Same type as [Melange_json.unknown_variant_case], re-exported under a name the derive layer can find helpers
    for. *)
type unknown = Melange_json.unknown_variant_case = {
  tag : string;
  payload : t list option;
}

let equal_unknown (a : unknown) (b : unknown) : bool =
  String.equal a.tag b.tag
  &&
  match a.payload, b.payload with
  | None, None -> true
  | Some xs, Some ys -> (try List.for_all2 equal xs ys with Invalid_argument _uneven_lengths -> false)
  | None, Some _ | Some _, None -> false

let show_unknown (u : unknown) : string = Printf.sprintf "Unknown %S" u.tag
let pp_unknown fmt (u : unknown) = Format.pp_print_string fmt (show_unknown u)
