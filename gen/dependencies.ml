(* Which other domains a selection needs, so the user hears it once.
   - "$ref": "Network.ResourceType" in Fetch means Fetch needs Network
   - Network needs Security the same way, and so on down the chain
   Fetch alone -> "Fetch also needs DOM,Debugger,Emulation,IO,Network,Page,Runtime,Security" *)

let collect_referenced_domains (domain : Protocol.domain) =
  let pick_other_domain ref_string =
    match Protocol.parse_ref ~current:domain.name ref_string with
    | referenced, _type_id when String.equal referenced domain.name -> None
    | referenced, _type_id -> Some referenced
  in
  let fragments = domain.types @ domain.commands @ domain.events in
  List.concat_map (fun fragment -> Protocol.collect_refs fragment []) fragments
  |> List.filter_map pick_other_domain
  |> List.sort_uniq String.compare

let find_missing ~(all : Protocol.domain list) ~(selected : Protocol.domain list) =
  let find_domain name = List.find_opt (fun (domain : Protocol.domain) -> String.equal domain.name name) all in
  let rec close_selection known_domains domains_to_visit =
    match domains_to_visit with
    | [] -> known_domains
    | name :: rest ->
    match List.mem name known_domains, find_domain name with
    | true, _known_already -> close_selection known_domains rest
    | false, None -> close_selection known_domains rest
    | false, Some domain -> close_selection (name :: known_domains) (collect_referenced_domains domain @ rest)
  in
  let selected_names = List.map (fun (domain : Protocol.domain) -> domain.name) selected in
  let needed_domains = close_selection selected_names (List.concat_map collect_referenced_domains selected) in
  needed_domains |> List.filter (fun name -> not (List.mem name selected_names)) |> List.sort String.compare
