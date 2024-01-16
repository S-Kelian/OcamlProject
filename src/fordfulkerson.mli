open Lbl_flot
open Graph


(*val update_path_labels : path -> 'a graph -> int -> 'a graph *)

val ford_fulkerson_algo : lbl_flot graph -> id -> id -> int -> lbl_flot graph * int

(* string (1/2) <-> label flot (1, 2) *)

val string_graph_to_flot_graph: string graph -> lbl_flot graph
val flot_graph_to_string_graph:  lbl_flot graph-> string graph








