open Fordfulkerson
open Teamfile
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 4 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile team outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a game score table\n" ^
         "    🟄  team    : team name\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

(* Arguments are : infile(1) team choisi (2) outfile(3) *)

  let infile = Sys.argv.(1)
  and team = Sys.argv.(2)
  and outfile = Sys.argv.(3)
  in

  (* Open file *)

  Printf.printf "\n\n  🟄  Opening file %s\n%!" infile ;

  (* Read the graph from the file *)
  let (graph,(lt,lg)) = from_file_game infile team in
  Printf.printf "\n  🟄  Graph successfully created %s\n%!" infile ;

  let flot_graph = string_graph_to_flot_graph graph in
  Printf.printf "\n  🟄  Flot graph successfully created %s\n%!" infile ;
  export_game outfile (flot_graph_to_string_graph flot_graph ) ((node_team_nom lt team),(node_game_nom lg team));

  (* Compute the max flow *)

  let nb_node = List.length (node_team_nom lt team) + List.length (node_game_nom lg team) + 1 in 

  let (_,max) = ford_fulkerson_algo flot_graph 0 nb_node 0 in

  Printf.printf "\n 🟄  Max flow pour %s : %d\n%!" team max ;

  let sum_of_source = sum_of_source graph in

  Printf.printf "\n 🟄  Somme des arcs sortant de s : %d\n%!" sum_of_source ;

  if max < sum_of_source then    
    Printf.printf " \n  🟄  %s est éliminé\n%!" team
  else
    Printf.printf "\n  🟄  %s n'est pas éliminé\n%!" team