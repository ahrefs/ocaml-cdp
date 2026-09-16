(* Synthesizes a sample JSON value for a protocol type, following the schema:
   required record fields get sample values, optional fields are omitted.
   Omitting optionals is also what terminates recursion — the protocol's
   recursive types (DOM.Node, Runtime.StackTrace) always recurse through
   optional fields. Re-entering a type through required fields cannot
   produce a finite sample, so it fails loudly and the caller skips it. *)

open Protocol

let spf = Printf.sprintf

let rec of_type ~domains ~visiting ~domain (type_json : Json.t) : Json.t =
  match Util.member "$ref" type_json with
  | `String ref_string ->
    let dom, id = parse_ref ~current:domain ref_string in
    of_named ~domains ~visiting (dom, id)
  | `Null ->
    (match Util.member "type" type_json with
    | `String "string" ->
      (match Util.member "enum" type_json with
      | `List (first :: _rest) -> first
      | _no_enum -> `String "sample")
    | `String "integer" -> `Int 7
    | `String "number" -> `Float 1.5
    | `String "boolean" -> `Bool true
    | `String "binary" -> `String "c2FtcGxl"
    | `String ("any" | "object") -> `Assoc []
    | `String "array" -> `List [ of_type ~domains ~visiting ~domain (Util.member "items" type_json) ]
    | unexpected -> failwith (spf "cdp-gen: cannot synthesize a sample for %s" (Json.to_string unexpected)))
  | unexpected -> failwith (spf "cdp-gen: bad $ref in sample synthesis: %s" (Json.to_string unexpected))

and of_named ~domains ~visiting (dom, id) : Json.t =
  if List.mem (dom, id) visiting then failwith (spf "cdp-gen: required-field cycle through %s.%s" dom id);
  let domain_def =
    match List.find_opt (fun (candidate : domain) -> String.equal candidate.name dom) domains with
    | Some found -> found
    | None -> failwith (spf "cdp-gen: sample synthesis: domain %s is not generated" dom)
  in
  let type_def =
    match List.find_opt (fun type_def -> String.equal (jstr "id" type_def) id) domain_def.types with
    | Some found -> found
    | None -> failwith (spf "cdp-gen: sample synthesis: unknown type %s.%s" dom id)
  in
  of_def ~domains ~visiting:((dom, id) :: visiting) ~domain:dom type_def

and of_def ~domains ~visiting ~domain type_def : Json.t =
  match Util.member "properties" type_def with
  | `List props -> of_props ~domains ~visiting ~domain props
  | _no_properties -> of_type ~domains ~visiting ~domain type_def

and of_props ~domains ~visiting ~domain props : Json.t =
  `Assoc
    (props
    |> List.filter_map (fun prop ->
      match jbool "optional" prop with
      | true -> None
      | false -> Some (jstr "name" prop, of_type ~domains ~visiting ~domain prop)))
