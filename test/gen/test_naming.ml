(* Unit tests for Naming: protocol names -> OCaml names. *)

open Cdp_gen.Naming

let pass name = Printf.printf "PASS %s\n" name

let () =
  assert (camel_to_snake "targetInfo" = "target_info");
  assert (camel_to_snake "DOMSnapshot" = "dom_snapshot");
  assert (camel_to_snake "IOStream" = "io_stream");
  assert (camel_to_snake "TargetID" = "target_id");
  pass "camel_to_snake handles acronym runs"

let () =
  assert (sanitize_lower "type" = "type_");
  assert (sanitize_lower "end" = "end_");
  assert (sanitize_lower "url" = "url");
  pass "OCaml keywords get a trailing underscore"

let () =
  assert (module_of_domain "DOMSnapshot" = "Dom_snapshot");
  assert (file_of_domain "Network" = "cdp_network");
  assert (types_module_of_domain "Network" = "Cdp_network_types");
  assert (submodule_of_name "getResponseBody" = "Get_response_body");
  pass "domain, file, and submodule names"

let () =
  assert (constructor_of_enum_value "happy" = "Happy");
  assert (constructor_of_enum_value "very-sad" = "Very_sad");
  assert (constructor_of_enum_value "text/css" = "Text_css");
  assert (constructor_of_enum_value "-Infinity" = "Minus_Infinity");
  assert (constructor_of_enum_value "0" = "V0");
  (* names that would clash with Stdlib or our own Other fallback *)
  assert (constructor_of_enum_value "none" = "None_");
  assert (constructor_of_enum_value "error" = "Error_");
  assert (constructor_of_enum_value "other" = "Other_");
  pass "enum values become valid, clash-free constructors"

let () = print_endline "all naming tests passed"
