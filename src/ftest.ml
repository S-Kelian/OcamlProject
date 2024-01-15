
open Fordfulkerson
open Gfile


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) outfileApresAlgo(5) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
 (* and outfileApresAlgo = Sys.argv.(5)*)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)


  let graph = read_flot_graph_from_string_graph (from_file infile) in


  (* TEST 1 : Algo de Ford-Fulkerson*)
  (* NB : Constructeurs, Setteurs, et Getteurs ne seront pas testés *)
  Printf.printf " Debut de l'algorithme de Ford-Fulkerson \n";
  let (exitGraph, flot_max) = ford_fulkerson_algo graph _source _sink 0 in
  Printf.printf "Le flot max est de %d \n" flot_max;
  export outfile (export_string_graph_from_flot_graph exitGraph)


