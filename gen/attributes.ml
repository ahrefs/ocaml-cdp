(* The compiler attributes a generated item carries.
   - its protocol description as a doc comment, for Merlin and odoc
   - deprecated / experimental / redirect flags as alerts
   Four shapes, one per place they are attached to:
   - render_item: [@@...] after a type, module, command or event
   - render_field: [@...] after a record field
   - render_domain: [@@...] after a domain alias in the index
   - render_file: [@@@...] at the top of a domain file *)

let render_flags ~marker ~separator flags =
  let render_alert { Model.Flags.attribute; message } =
    Printf.sprintf "%s[%s%s \"%s\"]" separator marker attribute message
  in
  String.concat "" (List.map render_alert (Model.Flags.to_alerts flags))

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

let render_item ~description ~flags =
  render_doc ~marker:"@@" ~separator:"\n" description ^ render_flags ~marker:"@@" ~separator:"\n" flags

let render_field ~description ~flags =
  render_flags ~marker:"@" ~separator:" " flags ^ render_doc ~marker:"@" ~separator:" " description

let render_domain (domain : Model.Domain.t) =
  render_doc ~marker:"@@" ~separator:"\n" domain.description ^ render_flags ~marker:"@@" ~separator:"\n" domain.flags

let render_file (domain : Model.Domain.t) =
  let rendered_doc = render_doc ~marker:"@@@" ~separator:"\n" domain.description in
  let rendered_flags = render_flags ~marker:"@@@" ~separator:"\n" domain.flags in
  let attributes = rendered_doc ^ rendered_flags in
  match attributes with
  | "" -> ""
  | attrs -> Printf.sprintf "%s\n\n" (String.trim attrs)
