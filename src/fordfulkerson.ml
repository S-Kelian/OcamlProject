open Lbl_flot
open Path
open Tools


(*path:le chemin augmentant*)
(*Si l'arête actuelle est dans le chemin augmentant, on soustrait la capacité minimale de la capacité existante (capacité - min_capacity). *)

(*algo principale*)
let rec ford_fulkerson_algo graph src tgt flot_tot = 
  let ecart_graph = find_ecart_graph graph in
  let path_found  = find_path ecart_graph src tgt [] in
    match path_found with
    | None -> (graph, flot_tot)
    | Some path_found -> print_path path_found;                        
      let flot = min_flow_of_path path_found ecart_graph max_int in 
      let newGraph = update_flot graph path_found flot in
      ford_fulkerson_algo newGraph src tgt (flot_tot + flot)
    ;;

(* Convert string (1/2) en label flot (1, 2) *)
let string_graph_to_flot_graph graph = gmap graph string_to_lblflot
let flot_graph_to_string_graph graph = gmap graph lblflot_to_string




  