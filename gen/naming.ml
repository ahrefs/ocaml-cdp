(* https://ocaml.org/manual/5.4/lex.html#sss:keywords *)
let keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "effect";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let is_upper character = character >= 'A' && character <= 'Z'
let is_lower character = character >= 'a' && character <= 'z'
let is_digit character = character >= '0' && character <= '9'
let is_letter character = is_upper character || is_lower character

(* A capital starts a new word when:
   - the char before it is lowercase or a digit
   - the char after it is lowercase
   FrameId -> frame_id, DOMSnapshot -> dom_snapshot, HTTP2Settings -> http2_settings *)
let camel_to_snake name =
  let buffer = Buffer.create (String.length name * 2) in
  let add_character position character =
    match is_upper character with
    | false -> Buffer.add_char buffer character
    | true ->
      let after_word_end = position > 0 && (is_lower name.[position - 1] || is_digit name.[position - 1]) in
      let before_lowercase = position < String.length name - 1 && is_lower name.[position + 1] in
      let starts_word = position > 0 && (after_word_end || before_lowercase) in
      (match starts_word with
      | false -> ()
      | true -> Buffer.add_char buffer '_');
      Buffer.add_char buffer (Char.lowercase_ascii character)
  in
  String.iteri add_character name;
  Buffer.contents buffer

let sanitize_lower name =
  let snake = camel_to_snake name in
  match List.mem snake keywords with
  | false -> snake
  | true -> snake ^ "_"

let module_of_domain domain = String.capitalize_ascii (sanitize_lower domain)

(* generated files are cdp_-prefixed so the library can be (wrapped false);
   the cdp.ml index re-exposes them as Cdp.Network etc. *)
let file_of_domain domain = "cdp_" ^ sanitize_lower domain
let domain_module_of_domain domain = String.capitalize_ascii (file_of_domain domain)
let types_module_of_domain domain = domain_module_of_domain domain ^ "_types"
let submodule_of_name name = String.capitalize_ascii (sanitize_lower name)

(* enum value -> constructor: 
   - "optionally-blockable" -> Optionally_blockable
   - "text/css" -> Text_css
   - "-Infinity" -> Minus_Infinity
   - "0" -> V0 *)
let constructor_of_enum_value value =
  let value =
    match String.starts_with ~prefix:"-" value with
    | false -> value
    | true -> "Minus_" ^ String.sub value 1 (String.length value - 1)
  in
  let to_identifier_char character =
    match is_letter character || is_digit character || Char.equal character '_' with
    | true -> character
    | false -> '_'
  in
  let name = String.map to_identifier_char value in
  let starts_with_digit = String.length name > 0 && is_digit name.[0] in
  let name =
    match starts_with_digit with
    | false -> name
    | true -> "V" ^ name
  in
  let name = String.capitalize_ascii name in
  match name with
  | "None" | "Some" | "Ok" | "Error" | "Other" -> name ^ "_"
  | name -> name
