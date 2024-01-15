open Lbl_flot
open Graph


(*val update_path_labels : path -> 'a graph -> int -> 'a graph *)

val ford_fulkerson_algo : lbl_flot graph -> id -> id -> int -> lbl_flot graph * int

(* Convert a string (1/2) en label flot (1, 2) *)

val read_flot_graph_from_string_graph: string graph -> lbl_flot graph
val export_string_graph_from_flot_graph:  lbl_flot graph-> string graph








