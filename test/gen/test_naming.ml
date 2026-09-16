(* Unit tests for Naming: protocol names -> OCaml names. *)

open Cdp_gen.Naming

let pass name = Printf.printf "PASS %s\n" name

let () =
  assert (String.equal (camel_to_snake "targetInfo") "target_info");
  assert (String.equal (camel_to_snake "DOMSnapshot") "dom_snapshot");
  assert (String.equal (camel_to_snake "IOStream") "io_stream");
  assert (String.equal (camel_to_snake "TargetID") "target_id");
  pass "camel_to_snake handles acronym runs"

let () =
  assert (String.equal (camel_to_snake "HTTP2") "http2");
  assert (String.equal (camel_to_snake "SHA256") "sha256");
  assert (String.equal (camel_to_snake "HTTP2Settings") "http2_settings");
  assert (String.equal (camel_to_snake "sha256Hash") "sha256_hash");
  pass "camel_to_snake keeps digits inside their word"

let () =
  assert (String.equal (sanitize_lower "type") "type_");
  assert (String.equal (sanitize_lower "end") "end_");
  assert (String.equal (sanitize_lower "url") "url");
  pass "OCaml keywords get a trailing underscore"

let () =
  assert (String.equal (module_of_domain "DOMSnapshot") "Dom_snapshot");
  assert (String.equal (file_of_domain "Network") "cdp_network");
  assert (String.equal (types_module_of_domain "Network") "Cdp_network_types");
  assert (String.equal (submodule_of_name "getResponseBody") "Get_response_body");
  pass "domain, file, and submodule names"

let () =
  assert (String.equal (constructor_of_enum_value "happy") "Happy");
  assert (String.equal (constructor_of_enum_value "very-sad") "Very_sad");
  assert (String.equal (constructor_of_enum_value "text/css") "Text_css");
  assert (String.equal (constructor_of_enum_value "-Infinity") "Minus_Infinity");
  assert (String.equal (constructor_of_enum_value "0") "V0");
  (* names that would clash with Stdlib or our own Other fallback *)
  assert (String.equal (constructor_of_enum_value "none") "None_");
  assert (String.equal (constructor_of_enum_value "error") "Error_");
  assert (String.equal (constructor_of_enum_value "other") "Other_");
  pass "enum values become valid, clash-free constructors"

let () = print_endline "all naming tests passed"
