open Graph

type path = id list

val find_path : int graph -> id -> id -> id list -> path option

val find_arc_in_path : 'a graph -> id list -> id -> id -> 'a arc option

val min_flow_of_path : id list -> int graph -> int -> int

val print_path : id list -> unit