open Graph

type 'a path = (id * id * 'a) list

val find_path : 'a graph -> id -> id -> id list -> id list

