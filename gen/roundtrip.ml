(* Prints the roundtrip test: every generated type must survive decode, encode, decode.
   - one check per receipt line with a sample: types, params and result records
   - one check per receipt line with enum values, over every wire value
   - a type whose sample cannot be built (required-field cycle) is skipped and reported
   The names come from Emit's receipt, so this file never works a name out itself. *)

type output = {
  contents : string;
  emitted : int;
  enum_checks : int;
  skipped : (string * string) list; (* label, reason *)
}

let render_enum_check (codec : Model.Codec.t) =
  let quoted_values = String.concat "; " (List.map (Printf.sprintf "%S") codec.enum_values) in
  let of_json = Model.Codec.of_json_path codec in
  let to_json = Model.Codec.to_json_path codec in
  let is_other =
    Printf.sprintf "(fun (value : %s.%s) -> match value with %s.Other _ -> true | _known -> false)" codec.module_path
      codec.type_name codec.module_path
  in
  Printf.sprintf "let () =\n  check_enum %S %s %s\n    %s\n    [ %s ]\n" codec.label of_json to_json is_other
    quoted_values

let render_check (codec : Model.Codec.t) sample =
  let raw = Yojson.Safe.to_string sample in
  let of_json = Model.Codec.of_json_path codec in
  let to_json = Model.Codec.to_json_path codec in
  let equal = Model.Codec.equal_path codec in
  Printf.sprintf "let () = check %S %s %s %s %S\n" codec.label of_json to_json equal raw

let emit ~revision ~type_index ~(codecs : Model.Codec.t list) =
  let buf = Buffer.create 65536 in
  Buffer.add_string buf (Emit.render_header ~revision);
  Buffer.add_string buf
    "(* Roundtrip tests over every generated type: decode a sample synthesized\n\
    \   from the protocol schema, encode it back, decode again, and compare.\n\
    \   The encoded JSON must also equal the input, keys sorted. *)\n\n\
     let failures = ref 0\n\n\
     let fail name what =\n\
    \  incr failures;\n\
    \  Printf.printf \"FAIL %s: %s\\n\" name what\n\n\
     let rec sort_keys (json : Yojson.Basic.t) : Yojson.Basic.t =\n\
    \  match json with\n\
    \  | `Assoc fields ->\n\
    \    `Assoc\n\
    \      (fields\n\
    \      |> List.sort (fun (left, _) (right, _) -> String.compare left right)\n\
    \      |> List.map (fun (key, value) -> key, sort_keys value))\n\
    \  | `List items -> `List (List.map sort_keys items)\n\
    \  | scalar -> scalar\n\n\
     let check name of_json to_json equal raw =\n\
    \  let json = Yojson.Basic.from_string raw in\n\
    \  let decoded = of_json json in\n\
    \  let encoded = to_json decoded in\n\
    \  (match Yojson.Basic.equal (sort_keys encoded) (sort_keys json) with\n\
    \  | true -> ()\n\
    \  | false -> fail name (\"encoded JSON differs from the input: \" ^ Yojson.Basic.to_string encoded));\n\
    \  let redecoded = of_json encoded in\n\
    \  match equal decoded redecoded with\n\
    \  | true -> ()\n\
    \  | false -> fail name \"value changed after an encode/decode roundtrip\"\n\n\
     (* every wire value of an enum decodes to its own constructor, never to\n\
    \   the Other fallback, and encodes back to the same string *)\n\
     let check_enum name of_json to_json is_other values =\n\
    \  List.iter\n\
    \    (fun value ->\n\
    \      let decoded = of_json (`String value) in\n\
    \      (match is_other decoded with\n\
    \      | false -> ()\n\
    \      | true -> fail name (\"value \" ^ value ^ \" decodes to Other\"));\n\
    \      match to_json decoded with\n\
    \      | `String encoded when String.equal encoded value -> ()\n\
    \      | encoded -> fail name (\"value \" ^ value ^ \" encodes back as \" ^ Yojson.Basic.to_string encoded))\n\
    \    values\n\n";
  let emitted = ref 0 in
  let enum_checks = ref 0 in
  let skipped = ref [] in
  let add_enum_check (codec : Model.Codec.t) =
    match codec.enum_values with
    | [] -> ()
    | _values ->
      incr enum_checks;
      Buffer.add_string buf (render_enum_check codec)
  in
  let add_check (codec : Model.Codec.t) =
    match codec.sample with
    | None -> ()
    | Some source ->
    match Sample.of_source ~type_index source with
    | sample ->
      incr emitted;
      Buffer.add_string buf (render_check codec sample)
    | exception Failure reason -> skipped := (codec.label, reason) :: !skipped
  in
  (* all enum checks first, then all roundtrip checks *)
  List.iter add_enum_check codecs;
  List.iter add_check codecs;
  Buffer.add_string buf
    "\n\
     let () =\n\
    \  match !failures with\n\
    \  | 0 -> print_endline \"all roundtrip tests passed\"\n\
    \  | count -> failwith (Printf.sprintf \"%d roundtrip failures\" count)\n";
  { contents = Buffer.contents buf; emitted = !emitted; enum_checks = !enum_checks; skipped = List.rev !skipped }
