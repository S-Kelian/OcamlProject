open Lbl_flot
open Path
open Tools
open Gfile
open Graph


(*TODO: update the current_flot (augment or reduce) of all arcs found in this path  *)
(*p:le chemin augmentant*)
(*Si l'arête actuelle est dans le chemin augmentant, on soustrait la capacité minimale de la capacité existante (capacité - min_capacity). *)

(* Modifie les label du graph à partir du maxflow du chemin path*)
let update_path_labels path graph flow =
  let cloneGraph = clone_nodes graph in
  let intGraph = gmap cloneGraph (fun x -> int_of_string x) in
  let rec add_newArcs path graphRef flow graph =
    match path with
    | [] -> graph
    | _ :: [] -> graph
    | first :: second :: rest ->
      let arc = find_arc graph first second in
      match arc with
      | None -> failwith "Arc not found"
      | Some a ->
        if a.lbl - flow > 0 then
          let newGraph = add_arc graph a.src a.tgt (a.lbl-flow)  in
          add_newArcs (second :: rest) graph flow newGraph
        else
          add_newArcs (second :: rest) graph flow graph
  in add_newArcs path intGraph flow intGraph





(*algo principale*)
let rec ford_fulkerson_algo graph src tgt = 
 (*  let ecart_graph = find_ecart_graph graph in*) 
    let path  = find_path graph src tgt [] in
     match path with
     | None -> graph
     | Some p -> 
        let flot = max_flow_of_path p graph 99 in 
        let newGraph = update_path_labels p graph flot in
        ford_fulkerson_algo newGraph src tgt
     ;;

       (*algo avec les labels spéciaux*)
let rec ford_fulkerson_algo_lblflot graph src tgt = 
  let ecart_graph = find_ecart_graph graph in
  let path  = find_path ecart_graph src tgt [] in
   match is_path_empty path with
   | None -> graph
   | Some p -> 
      let flot = max_flow_of_path_cap p graph 99 in 
      let newGraph = update_path_labels_flot p ecart_graph flot in 
      ford_fulkerson_algo_lblflot newGraph src tgt
;;

let is_path_empty path = (path = [])

  
let label_flot_to_string label = "(" ^ string_of_int label.flot_actuel ^ "/" ^ string_of_int label.cap ^ ")"

(* Convert a string (1/2) en label flot (1, 2) *)
let string_to_label_flot label = Scanf.sscanf label "(%d/%d)" (fun flot cap -> {flot_actuel = flot; cap = cap})

let read_flot_graph_from_string_graph graph = gmap graph string_to_label_flot;;
let export_string_graph_from_flot_graph graph = gmap graph label_flot_to_string


  