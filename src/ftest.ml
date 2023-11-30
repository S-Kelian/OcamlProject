open Gfile
open Tools

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


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
    let clc = clone_nodes graph in
    let aa = add_arc clc 0 2 12 in
    let ab = add_arc aa 0 1 9 in
    let ac = add_arc ab 1 3 10 in
    let ad = add_arc ac 2 6 20 in
    let ae = add_arc ad 2 5 14 in
    let af = add_arc ae 2 4 78 in

    let strG = gmap af string_of_int in

  (* Rewrite the graph that has been read. *)
  let () = export outfile strG in

  ()

