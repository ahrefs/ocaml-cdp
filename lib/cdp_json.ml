(* Hand-written JSON glue for the generated code.
   - raw JSON type
   - payload of the Other enum constructor
   - message repairs: lone surrogates, big integers *)

(** A raw JSON value. Used for protocol fields typed "any" or bare "object". *)
type t = Jsonkit.t

(* identity codecs: lets [@@deriving json] work on fields typed [Cdp_json.t] *)
let of_json (json : Jsonkit.t) : t = json
let to_json (value : t) : Jsonkit.t = value

(* derive layer for [Cdp_json.t] fields: [@@deriving show, eq] looks these up *)
let equal : t -> t -> bool = Yojson.Basic.equal
let show (value : t) : string = Yojson.Basic.to_string value
let pp fmt (value : t) = Format.pp_print_string fmt (show value)

(** A protocol [number]. JavaScript has NaN and the infinities, JSON does not, so Chrome writes them as [null].
    Decoding [null] gives [nan] instead of failing the whole message; encoding a non-finite float gives [null] back. *)
type number = float

let number_of_json (json : t) : number =
  match json with
  | `Null -> Float.nan
  | finite -> Jsonkit.Primitives.float_of_json finite

let number_to_json (value : number) : t =
  match Float.is_finite value with
  | true -> Jsonkit.Primitives.float_to_json value
  | false -> `Null

let equal_number : number -> number -> bool = Float.equal
let pp_number fmt (value : number) = Format.pp_print_float fmt value
let show_number (value : number) : string = Float.to_string value

(** Payload of catch-all [Other] constructors: an enum value this protocol revision does not know. [tag] is the raw wire
    string. Same type as [Jsonkit.unknown_variant_case], re-exported under a name the derive layer can find helpers for.
*)
type unknown = Jsonkit.unknown_variant_case = {
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

let hex_value ch =
  match ch with
  | '0' .. '9' -> Some (Char.code ch - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code ch - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code ch - Char.code 'A' + 10)
  | _not_hex -> None

(* the UTF-16 code unit of a [\uXXXX] escape starting at [position], if one is there *)
let unicode_escape_at raw position =
  match String.length raw >= position + 6 && raw.[position] = '\\' && raw.[position + 1] = 'u' with
  | false -> None
  | true ->
    let digit offset = hex_value raw.[position + 2 + offset] in
    (match digit 0, digit 1, digit 2, digit 3 with
    | Some d0, Some d1, Some d2, Some d3 -> Some ((((((d0 * 16) + d1) * 16) + d2) * 16) + d3)
    | _some_digit_not_hex -> None)

let is_high_surrogate code = code >= 0xD800 && code <= 0xDBFF
let is_low_surrogate code = code >= 0xDC00 && code <= 0xDFFF

(* every surrogate escape's first hex digit is d/D (U+D800..U+DFFF), so a
   backslash-u-d scan gates the repair; memchr makes it cheap on the huge
   backslash-free payloads (base64 screenshots) *)
let rec contains_surrogate_escape raw position =
  match String.index_from_opt raw position '\\' with
  | None -> false
  | Some backslash ->
    backslash + 2 < String.length raw
    && raw.[backslash + 1] = 'u'
    && (raw.[backslash + 2] = 'd' || raw.[backslash + 2] = 'D')
    || contains_surrogate_escape raw (backslash + 1)

let repair_all_surrogates raw =
  let length = String.length raw in
  let buf = Buffer.create length in
  let rec copy_from position =
    match position >= length with
    | true -> ()
    | false ->
    match raw.[position] with
    | '\\' when position + 1 < length && raw.[position + 1] = '\\' ->
      (* escaped backslash: what follows is literal text, not an escape *)
      Buffer.add_string buf "\\\\";
      copy_from (position + 2)
    | '\\' ->
      (match unicode_escape_at raw position with
      | Some code when is_high_surrogate code ->
        (match unicode_escape_at raw (position + 6) with
        | Some next when is_low_surrogate next ->
          Buffer.add_string buf (String.sub raw position 12);
          copy_from (position + 12)
        | _unpaired_high ->
          Buffer.add_string buf "\\ufffd";
          copy_from (position + 6))
      | Some code when is_low_surrogate code ->
        (* a low surrogate first: any paired one was consumed above *)
        Buffer.add_string buf "\\ufffd";
        copy_from (position + 6)
      | _not_a_surrogate_escape ->
        let step = min 2 (length - position) in
        Buffer.add_string buf (String.sub raw position step);
        copy_from (position + step))
    | plain ->
      Buffer.add_char buf plain;
      copy_from (position + 1)
  in
  copy_from 0;
  Buffer.contents buf

(** Chrome escapes every non-ASCII UTF-16 code unit as [\uXXXX] without checking surrogate pairing, and JavaScript
    strings may hold unpaired surrogates (an emoji cut in half by [substring], a truncated title). A lone HIGH surrogate
    makes a strict parser reject the whole message; a lone LOW surrogate is worse — Yojson accepts it and produces
    invalid UTF-8 bytes. So this must run BEFORE parsing, on every message: it replaces each unpaired surrogate escape
    with [�] (the replacement character). Literal text like [\\ud800] (an escaped backslash) is left untouched, and
    input without surrogate escapes is returned as-is, without a copy. *)
let repair_lone_surrogates raw =
  match contains_surrogate_escape raw 0 with
  | false -> raw
  | true -> repair_all_surrogates raw

(** Convert a [Yojson.Safe] tree to [Yojson.Basic], turning integers past OCaml's 63 bits ([`Intlit]) into floats —
    which is what they were in JavaScript, where every number is a float. The strict [Basic] parser rejects such
    integers outright, dropping the whole message; parsing with [Safe] and converting keeps it. *)
let rec basic_of_safe (json : Yojson.Safe.t) : t =
  match json with
  | `Intlit big_integer -> `Float (float_of_string big_integer)
  | `Assoc fields -> `Assoc (List.map (fun (key, value) -> key, basic_of_safe value) fields)
  | `List items -> `List (List.map basic_of_safe items)
  | `Null -> `Null
  | `Bool flag -> `Bool flag
  | `Int number -> `Int number
  | `Float number -> `Float number
  | `String text -> `String text
