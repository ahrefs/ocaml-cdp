type 'item t = 'item list

let make ~first ~others = first :: others

let of_list items =
  match items with
  | [] -> failwith "Non_empty_list.of_list: the list is empty"
  | _first :: _others -> items

let of_list_opt items =
  match items with
  | [] -> None
  | _first :: _others -> Some items

let to_list items = items

let head items = List.hd items

let length items = List.length items

let tail items = List.tl items
