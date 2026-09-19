(* Which other domains a selection needs, so the user hears it once.
   - "$ref": "Network.ResourceType" in Fetch means Fetch needs Network
   - Network needs Security the same way, and so on down the chain
   Fetch alone -> "Fetch also needs DOM,Debugger,Emulation,IO,Network,Page,Runtime,Security" *)

let collect_referenced_domains (domain : Model.Domain.t) =
  let pick_other_domain (type_ref : Model.Type_ref.t) =
    match String.equal type_ref.domain domain.name with
    | true -> None
    | false -> Some type_ref.domain
  in
  Model.Domain.collect_refs domain |> List.filter_map pick_other_domain |> List.sort_uniq String.compare

let find_missing ~(all : Model.Domain.t list) ~(selected : Model.Domain.t list) =
  let find_domain name = List.find_opt (fun (domain : Model.Domain.t) -> String.equal domain.name name) all in
  let rec close_selection known_domains domains_to_visit =
    match domains_to_visit with
    | [] -> known_domains
    | name :: rest ->
    match List.mem name known_domains, find_domain name with
    | true, _known_already -> close_selection known_domains rest
    | false, None -> close_selection known_domains rest
    | false, Some domain -> close_selection (name :: known_domains) (collect_referenced_domains domain @ rest)
  in
  let selected_names = List.map (fun (domain : Model.Domain.t) -> domain.name) selected in
  let needed_domains = close_selection selected_names (List.concat_map collect_referenced_domains selected) in
  needed_domains |> List.filter (fun name -> not (List.mem name selected_names)) |> List.sort String.compare

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
    let pick_other_domain (type_ref : Model.Type_ref.t) =
      match String.equal type_ref.domain domain.name || is_sealed type_ref with
      | true -> None
      | false -> Some type_ref.domain
    in
    List.concat_map Model.Type_def.collect_refs domain.types
    |> List.filter_map pick_other_domain
    |> List.sort_uniq String.compare
  in
  let edges = Hashtbl.create 16 in
  List.iter (fun (domain : Model.Domain.t) -> Hashtbl.replace edges domain.name (edges_of_domain domain)) domains;
  let visiting = Hashtbl.create 16 in
  let finished = Hashtbl.create 16 in
  let rec visit path node =
    match Hashtbl.mem finished node, Hashtbl.mem visiting node with
    | true, _finished_first -> ()
    | false, true ->
      let cycle = String.concat " -> " (List.rev (node :: path)) in
      failwith
        (Printf.sprintf
           "cdp-gen: type-level cycle between domains: %s. Sealed-alias routing through Base cannot break it; the \
            domains in the cycle must be merged into one compilation unit."
           cycle)
    | false, false ->
      Hashtbl.replace visiting node ();
      List.iter (visit (node :: path)) (Option.value (Hashtbl.find_opt edges node) ~default:[]);
      Hashtbl.remove visiting node;
      Hashtbl.replace finished node ()
  in
  List.iter (fun (domain : Model.Domain.t) -> visit [] domain.name) domains
