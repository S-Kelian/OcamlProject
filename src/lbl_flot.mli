(*open Path*)
open Graph


type lbl_flot

val string_lbl_flot : lbl_flot -> string
val get_lbl : 'a arc -> 'a

val update_path_labels_flot: id list  -> lbl_flot graph -> int -> lbl_flot graph
val max_flow_of_path_cap  : id list -> lbl_flot graph -> int -> int
val find_ecart_arc : lbl_flot arc -> lbl_flot arc
val find_ecart_graph : lbl_flot graph -> id graph





