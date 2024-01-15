open Fordfulkerson
open Teamfile
open Gfile

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


  (* Arguments are : infile(1) outfile(3) *)

  let infile = Sys.argv.(1)
  and team = Sys.argv.(2)
  and outfile = Sys.argv.(3)
  in

  (* Open file *)

  Printf.printf "\n\n  🟄  Opening file %s\n%!" infile ;

  (* Read the graph from the file *)
  let graph = from_file_game infile team in
  Printf.printf "\n  🟄  Graph successfully created %s\n%!" infile ;

  (* NB : Constructeurs, Setteurs, et Getteurs ne seront pas testÃ©s *)

  let flot_graph = read_flot_graph_from_string_graph graph in
  Printf.printf "\n  🟄  Flot graph successfully created %s\n%!" infile ;
  export outfile (export_string_graph_from_flot_graph flot_graph);

  (* Compute the max flow *)

  let (_,max) = ford_fulkerson_algo flot_graph 0 7 0 in

  Printf.printf "\n 🟄  Max flow pour %s : %d\n%!" team max ;

  let sum_of_source = sum_of_source graph in

  if sum_of_source < max then
    Printf.printf "\n  🟄  %s est éliminé\n%!" team
  else
    Printf.printf "\n  🟄  %s n'est pas éliminé\n%!" team