open Graph
open Gfile

type team
type game 

type name = string

val node_team_nom : team list -> string -> string list 
val node_game_nom : game list -> name -> name list 
val construct_graph :  team list * game list -> name -> string graph
val from_file_game : path -> string -> (name graph * (team list * game list))
val export_game: path -> string graph -> (string list * string list) -> unit
val sum_of_source : string graph -> int


