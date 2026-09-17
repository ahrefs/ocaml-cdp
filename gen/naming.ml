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

let is_upper ch = ch >= 'A' && ch <= 'Z'
let is_lower ch = ch >= 'a' && ch <= 'z'
let is_digit ch = ch >= '0' && ch <= '9'

(* A capital starts a new word when:
   - the char before it is lowercase or a digit
   - the char after it is lowercase
   FrameId -> frame_id, DOMSnapshot -> dom_snapshot, HTTP2Settings -> http2_settings *)
let camel_to_snake name =
  let buf = Buffer.create (String.length name * 2) in
  String.iteri
    (fun pos ch ->
      if is_upper ch then begin
        let after_word_end = pos > 0 && (is_lower name.[pos - 1] || is_digit name.[pos - 1]) in
        let before_lowercase = pos < String.length name - 1 && is_lower name.[pos + 1] in
        if pos > 0 && (after_word_end || before_lowercase) then Buffer.add_char buf '_';
        Buffer.add_char buf (Char.lowercase_ascii ch)
      end
      else Buffer.add_char buf ch)
    name;
  Buffer.contents buf

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

(* enum value -> constructor: "optionally-blockable" -> Optionally_blockable,
   "text/css" -> Text_css, "-Infinity" -> Minus_Infinity, "0" -> V0 *)
let constructor_of_enum_value value =
  let value =
    match String.starts_with ~prefix:"-" value with
    | false -> value
    | true -> "Minus_" ^ String.sub value 1 (String.length value - 1)
  in
  let identifier_char ch = if is_lower ch || is_upper ch || is_digit ch || ch = '_' then ch else '_' in
  let name = String.map identifier_char value in
  let name = if String.length name > 0 && is_digit name.[0] then "V" ^ name else name in
  let name = String.capitalize_ascii name in
  match name with
  | "None" | "Some" | "Ok" | "Error" | "Other" -> name ^ "_"
  | name -> name
