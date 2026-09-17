(* The compiler attributes a generated item carries.
   - its protocol description as a doc comment, for Merlin and odoc
   - deprecated / experimental / redirect flags as alerts
   Four shapes, one per place they are attached to:
   - render_item: [@@...] after a type, module, command or event
   - render_field: [@...] after a record field
   - render_domain: [@@...] after a domain alias in the index
   - render_file: [@@@...] at the top of a domain file *)

let render_flags ~marker ~separator { Protocol.deprecated; experimental; redirect } =
  let attribute name message = Printf.sprintf "%s[%s%s \"%s\"]" separator marker name message in
  let deprecated_attr =
    match deprecated, redirect with
    | false, None -> ""
    | true, None -> attribute "ocaml.deprecated" "deprecated in CDP"
    | true, Some domain ->
      attribute "ocaml.deprecated" (Printf.sprintf "deprecated in CDP, redirected to the %s domain" domain)
    | false, Some domain -> attribute "alert redirected" (Printf.sprintf "redirected to the %s domain in CDP" domain)
  in
  let experimental_attr =
    match experimental with
    | false -> ""
    | true -> attribute "alert experimental" "experimental in CDP, may change with Chrome"
  in
  deprecated_attr ^ experimental_attr

(* odoc reads { } [ ] @ as markup, so a description escapes them *)
let escape_odoc text =
  let escape ch =
    match ch with
    | '{' | '}' | '[' | ']' | '@' | '\\' -> Printf.sprintf "\\%c" ch
    | plain -> String.make 1 plain
  in
  String.concat "" (List.map escape (List.of_seq (String.to_seq text)))

let render_doc ~marker ~separator description =
  match description with
  | None -> ""
  | Some text -> Printf.sprintf "%s[%socaml.doc %S]" separator marker (escape_odoc text)

let render_item json =
  render_doc ~marker:"@@" ~separator:"\n" (Protocol.description_of_json json)
  ^ render_flags ~marker:"@@" ~separator:"\n" (Protocol.flags_of_json json)

let render_field json =
  render_flags ~marker:"@" ~separator:" " (Protocol.flags_of_json json)
  ^ render_doc ~marker:"@" ~separator:" " (Protocol.description_of_json json)

let render_domain (domain : Protocol.domain) =
  render_doc ~marker:"@@" ~separator:"\n" domain.description ^ render_flags ~marker:"@@" ~separator:"\n" domain.flags

let render_file (domain : Protocol.domain) =
  let attributes =
    render_doc ~marker:"@@@" ~separator:"\n" domain.description
    ^ render_flags ~marker:"@@@" ~separator:"\n" domain.flags
  in
  match attributes with
  | "" -> ""
  | attrs -> Printf.sprintf "%s\n\n" (String.trim attrs)
