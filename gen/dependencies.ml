(* Which other domains a selection needs, so the user hears it once.
   - "$ref": "Network.ResourceType" in Fetch means Fetch needs Network
   - Network needs Security the same way, and so on down the chain
   Fetch alone -> "Fetch also needs DOM,Debugger,Emulation,IO,Network,Page,Runtime,Security" *)

(* the other domains these refs point into: Fetch's refs give ["Network"; "Page"; ...], never "Fetch" *)
let collect_other_domains ~(domain : Model.Domain.t) type_refs =
  let pick_other_domain (type_ref : Model.Type_ref.t) =
    match String.equal type_ref.domain domain.name with
    | true -> None
    | false -> Some type_ref.domain
  in
  type_refs |> List.filter_map pick_other_domain |> List.sort_uniq String.compare

let collect_referenced_domains (domain : Model.Domain.t) =
  Model.Domain.collect_refs domain |> collect_other_domains ~domain

let find_missing_domains ~(all_domains : Model.Domain.t list) ~(selected_domains : Model.Domain.t list) =
  let find_domain name = Model.Domain.find_by_name all_domains name in
  let rec close_selection known_names names_to_visit =
    match names_to_visit with
    | [] -> known_names
    | name :: rest ->
    match List.exists (String.equal name) known_names, find_domain name with
    | true, _known_already -> close_selection known_names rest
    | false, None -> close_selection known_names rest
    | false, Some domain -> close_selection (name :: known_names) (collect_referenced_domains domain @ rest)
  in
  let selected_names = List.map (fun (domain : Model.Domain.t) -> domain.name) selected_domains in
  let needed_names = close_selection selected_names (List.concat_map collect_referenced_domains selected_domains) in
  needed_names
  |> List.filter (fun name -> not (List.exists (String.equal name) selected_names))
  |> List.sort String.compare

(* a $ref to a type that exists nowhere would surface much later, as a compile
   error inside generated code. refused with the referrer's name instead *)
let check_refs_exist ~type_index (domains : Model.Domain.t list) =
  let check ~owner type_refs =
    let check_ref (type_ref : Model.Type_ref.t) =
      match Model.Type_index.find type_index type_ref with
      | Some _found -> ()
      | None ->
        failwith (Printf.sprintf "cdp-gen: %s references %s.%s, which does not exist" owner type_ref.domain type_ref.id)
    in
    List.iter check_ref type_refs
  in
  let check_domain (domain : Model.Domain.t) =
    let owner name = Printf.sprintf "%s.%s" domain.name name in
    List.iter
      (fun (type_def : Model.Type_def.t) -> check ~owner:(owner type_def.id) (Model.Type_def.collect_refs type_def))
      domain.types;
    List.iter
      (fun (command : Model.Command.t) -> check ~owner:(owner command.name) (Model.Command.collect_refs command))
      domain.commands;
    List.iter
      (fun (event : Model.Event.t) -> check ~owner:(owner event.name) (Model.Event.collect_refs event))
      domain.events
  in
  List.iter check_domain domains

(* Type files of two domains must not need each other: OCaml modules cannot form a loop.
   - refs to sealed aliases do not count: they go through Cdp_base, a leaf
   - commands and events never count: they live in the domain file, which only points at type files *)
let check_no_type_loop ~type_index (domains : Model.Domain.t list) =
  let is_sealed type_ref =
    Option.is_some (Option.bind (Model.Type_index.find type_index type_ref) Model.Type_def.sealed_primitive_of)
  in
  let edges_of_domain (domain : Model.Domain.t) =
    List.concat_map Model.Type_def.collect_refs domain.types
    |> List.filter (fun type_ref -> not (is_sealed type_ref))
    |> collect_other_domains ~domain
  in
  let edges = Hashtbl.create 16 in
  List.iter (fun (domain : Model.Domain.t) -> Hashtbl.replace edges domain.name (edges_of_domain domain)) domains;
  let visiting = Hashtbl.create 16 in
  let finished = Hashtbl.create 16 in
  let rec visit path node =
    match Hashtbl.mem finished node, Hashtbl.mem visiting node with
    | true, _is_visiting -> ()
    | false, true ->
      let cycle = String.concat " -> " (List.rev (node :: path)) in
      failwith
        (Printf.sprintf
           "cdp-gen: the types of %s point at each other in a loop. OCaml cannot compile files that need each other, \
            so cdp-gen cannot generate these domains yet"
           cycle)
    | false, false ->
      Hashtbl.replace visiting node ();
      List.iter (visit (node :: path)) (Option.value (Hashtbl.find_opt edges node) ~default:[]);
      Hashtbl.remove visiting node;
      Hashtbl.replace finished node ()
  in
  List.iter (fun (domain : Model.Domain.t) -> visit [] domain.name) domains
