(* repair_lone_surrogates: unpaired \uXXXX surrogate escapes become the
   replacement-character escape, everything else survives byte for byte.
   Escape sequences are built with [esc] so the source stays plain ASCII. *)

let pass name = Printf.printf "PASS %s\n" name
let repair = Cdp_json.repair_lone_surrogates

(* the six wire characters of a \uXXXX escape *)
let esc code = Printf.sprintf "\\u%04x" code
let high = esc 0xd83d
let low = esc 0xde00
let replacement = "\\ufffd"
let quoted text = "\"" ^ text ^ "\""

let () =
  (* untouched inputs *)
  assert (repair "" = "");
  assert (repair {|{"id":1,"result":{"value":"plain"}}|} = {|{"id":1,"result":{"value":"plain"}}|});
  assert (repair {|"tab\tquote\"backslash\\"|} = {|"tab\tquote\"backslash\\"|});
  assert (repair (quoted (esc 0x4e2d)) = quoted (esc 0x4e2d));
  assert (repair "\"raw utf8 \240\159\152\128\"" = "\"raw utf8 \240\159\152\128\"");
  pass "plain json, ordinary escapes, non-surrogate \\u escapes, and raw utf-8 survive";

  (* a valid surrogate pair survives, either hex case *)
  assert (repair (quoted (high ^ low)) = quoted (high ^ low));
  assert (repair (quoted "\\uD83D\\uDE00") = quoted "\\uD83D\\uDE00");
  pass "valid surrogate pairs survive";

  (* the reproduced Chrome message: a lone high surrogate *)
  let envelope value = {|{"id":1,"result":{"result":{"type":"string","value":"|} ^ value ^ {|"}}}|} in
  let broken = envelope high in
  let repaired = repair broken in
  assert (repaired = envelope replacement);
  (match Yojson.Basic.from_string broken with
  | exception Yojson.Json_error _rejected -> ()
  | _parsed -> assert false);
  let module Util = Yojson.Basic.Util in
  let value =
    Yojson.Basic.from_string repaired |> Util.member "result" |> Util.member "result" |> Util.member "value"
  in
  assert (value = `String "\239\191\189");
  pass "the lone high surrogate Chrome really sends parses after repair";

  (* lone low surrogate, unpaired escape at the end of input, two lone highs *)
  assert (repair (quoted low) = quoted replacement);
  assert (repair ("\"x" ^ high) = "\"x" ^ replacement);
  assert (repair (quoted (high ^ high ^ "x")) = quoted (replacement ^ replacement ^ "x"));
  pass "lone low, unpaired tail, and doubled high surrogates each become the replacement";

  (* literal backslash text is NOT an escape: backslash-backslash-ud83d is an
     escaped backslash followed by five plain characters *)
  let looks_like_surrogate = quoted "literal \\\\ud83d text" in
  assert (repair looks_like_surrogate = looks_like_surrogate);
  pass "escaped-backslash text that merely looks like a surrogate survives";

  print_endline "all repair tests passed"
