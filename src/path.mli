open Graph

type path = id list

val find_path : int graph -> id -> id -> id list -> path option

val find_arc_in_path : 'a graph -> id list -> id -> id -> 'a arc option

val min_flow_of_path : id list -> int graph -> int -> int

(*val update_path_labels : id list -> 'a graph -> int -> 'a graph*)

(*val test_find_path_with_maxflow : string -> id -> id -> unit*)


