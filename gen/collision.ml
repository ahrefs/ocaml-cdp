(* the first two items that map to one key: fields "frameId" and "frame_id" both become frame_id *)
type 'item t = {
  earlier : 'item;
  later : 'item;
  key : string;
}

let find ~key items =
  let seen = Hashtbl.create 16 in
  let rec find_in remaining =
    match remaining with
    | [] -> None
    | item :: rest ->
      let item_key = key item in
      (match Hashtbl.find_opt seen item_key with
      | Some earlier -> Some { earlier; later = item; key = item_key }
      | None ->
        Hashtbl.replace seen item_key item;
        find_in rest)
  in
  find_in items
