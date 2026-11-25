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
  String.iteri
    (fun position character ->
      if is_upper character then begin
        let after_word_end = position > 0 && (is_lower name.[position - 1] || is_digit name.[position - 1]) in
        let before_lowercase = position < String.length name - 1 && is_lower name.[position + 1] in
        if position > 0 && (after_word_end || before_lowercase) then Buffer.add_char buffer '_';
        Buffer.add_char buffer (Char.lowercase_ascii character)
      end
      else Buffer.add_char buffer character)
    name;
  Buffer.contents buffer

let sanitize_lower name =
  let snake = camel_to_snake name in
  if List.mem snake keywords then snake ^ "_" else snake

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
  let identifier_char character =
    if is_lower character || is_upper character || is_digit character || character = '_' then character else '_'
  in
  let name = String.map identifier_char value in
  let name = if String.length name > 0 && is_digit name.[0] then "V" ^ name else name in
  let name = String.capitalize_ascii name in
  match name with
  | "None" | "Some" | "Ok" | "Error" | "Other" -> name ^ "_"
  | name -> name
