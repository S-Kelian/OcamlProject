
open Graph
open Gfile


type lbl_flot
type label_flot_cost

type flot_graph = lbl_flot graph
val string_lbl_flot : lbl_flot -> string
val update_path_labels : path -> 'a graph -> int -> 'a graph
val update_path_labels_flot: path -> lbl_flot graph -> int -> lbl_flot graph
val max_flow_of_path_cap  : id list -> lbl_flot graph -> int -> int
val find_ecart_arc : 'a arc -> 'a arc
val find_ecart_graph : lbl_flot graph -> id graph
val ford_fulkerson_algo : lbl_flot graph -> 'a -> 'b -> lbl_flot graph







