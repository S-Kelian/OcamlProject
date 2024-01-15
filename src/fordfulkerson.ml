open Lbl_flot
open Path
open Tools


(*TODO: update the current_flot (augment or reduce) of all arcs found in this path  *)
(*p:le chemin augmentant*)
(*Si l'arête actuelle est dans le chemin augmentant, on soustrait la capacité minimale de la capacité existante (capacité - min_capacity). *)

(*algo principale*)
let rec ford_fulkerson_algo graph src tgt flot_tot = 
  let ecart_graph = find_ecart_graph graph in
  let path_found  = find_path ecart_graph src tgt [] in
    match path_found with
    | None -> Printf.printf "Pas de chemin trouvé\n";
              (graph, flot_tot)
    | Some path_found -> Printf.printf "Chemin trouvé\n";
      print_path path_found;                        
      let flot = min_flow_of_path path_found ecart_graph max_int in 
      let newGraph = update_flot graph path_found flot in
      ford_fulkerson_algo newGraph src tgt (flot_tot + flot)
    ;;

(* Convert a string (1/2) en label flot (1, 2) *)

let read_flot_graph_from_string_graph graph = gmap graph string_to_label_flot
let export_string_graph_from_flot_graph graph = gmap graph label_flot_to_string




  