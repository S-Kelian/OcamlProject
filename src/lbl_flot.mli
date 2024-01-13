
open Graph
open Gfile


type lbl_flot

val string_lbl_flot : lbl_flot -> string

val update_path_labels_flot: path -> lbl_flot graph -> int -> lbl_flot graph
val max_flow_of_path_cap  : id list -> lbl_flot graph -> int -> int
val find_ecart_arc : 'a arc -> 'a arc



