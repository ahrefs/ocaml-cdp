(* a list with at least one item: the type says it, so no code has to handle [] *)

type 'item t = private 'item list

val make : first:'item -> others:'item list -> 'item t
val of_list : 'item list -> 'item t
val of_list_opt : 'item list -> 'item t option
val to_list : 'item t -> 'item list
val head : 'item t -> 'item
val length : 'item t -> int
val tail : 'item t -> 'item list
