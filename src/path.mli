open Graph

type path = id list

val find_path : int graph -> id -> id -> id list -> path option

val max_flow_of_path : id list -> int graph -> int -> int


