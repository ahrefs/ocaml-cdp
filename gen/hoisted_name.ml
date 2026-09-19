(* An enum written inside a property has no name. The generator moves it out
   into its own type; this module picks that type's name.
   - in a named type: <type>_<field>
   - in a command or event module: the field name, with the module name as
     prefix when a domain type has that name already

   Network.Request.referrerPolicy  -> request_referrer_policy
   Page.captureScreenshot params.format -> format
   Page.frameStartedNavigating params.navigationType -> frame_started_navigating_navigation_type
     (Page also has a type NavigationType) *)

(* <type>_<field>: two types of a domain may share a field name *)
let name_for_type_field ~type_id field =
  Printf.sprintf "%s_%s" (Naming.camel_to_snake type_id) (Naming.camel_to_snake field)

(* <type>_item: a named type that is itself an array of an inline enum *)
let name_for_array_item ~type_id = Printf.sprintf "%s_item" (Naming.camel_to_snake type_id)

let collect_domain_type_names (domain : Model.Domain.t) =
  List.map (fun (type_def : Model.Type_def.t) -> Naming.sanitize_lower type_def.id) domain.types

let name_for_item_field ~domain_type_names ~item_name field =
  let name = Naming.sanitize_lower field in
  match List.mem name domain_type_names with
  | false -> name
  | true -> Printf.sprintf "%s_%s" (Naming.sanitize_lower item_name) name
