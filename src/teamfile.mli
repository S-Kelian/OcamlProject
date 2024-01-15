open Graph
open Gfile

type team
type game 

type name = string

val construct_graph :  team list * game list -> name -> string graph
val from_file_game : path -> string -> string graph
val export_game: path -> string graph -> unit
val sum_of_source : string graph -> int