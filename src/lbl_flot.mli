(*open Path*)
open Graph
open Path


type lbl_flot

val lblflot_to_string: lbl_flot -> string
val string_to_lblflot: string -> lbl_flot 
val update_flot : lbl_flot graph -> path -> int -> lbl_flot graph
val find_ecart_arc : lbl_flot arc -> lbl_flot arc
val find_ecart_graph : lbl_flot graph -> id graph





