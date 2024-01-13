
open Graph
open Gfile
open Tools
open Path

(* Label spécifique pour les calcul de flot, qui contient le flot actuel et la capacité maximale*)
type lbl_flot = {
  flot_actuel : int;
  cap : int;
}

(* Getters pour lbl_flot*)
let get_flot_actuel lblf = lblf.flot_actuel;;
let get_cap lblf = lblf.cap;;

(* Retourne le lbl_flot sous forme de string : (flot_actuel/capacité)*)
let string_lbl_flot lbl = "(" ^ string_of_int lbl.flot_actuel ^ "/" ^ string_of_int lbl.cap ^ ")";;


type label_flot_cost = {
  current_flot: int;
  capacity: int;
  cost: int
}

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

(* Modifie les labels_flow d'un graph à partir du flot max*)
let update_path_labels_flot path graph flow =
  let cloneGraph = clone_nodes graph in
  (*let intGraph = gmap cloneGraph (fun x -> int_of_string x) in*)
  let rec add_newArcs path graphRef flow graph =
    match path with
    | [] -> graph
    | _ :: [] -> graph
    | first :: second :: rest ->
      let arc = find_arc graph first second in
      match arc with
      | None -> failwith "Arc not found"
      | Some a ->
        if a.lbl.cap - a.lbl.flot_actuel - flow > 0 then
          let flotA = a.lbl.flot_actuel + flow in
          let labelflot = {flot_actuel = flotA ; cap = a.lbl.cap} in
          let newA  = { src = a.src ; tgt = a.tgt ; lbl= labelflot} in 
          let newGraph = new_arc graph newA  in
          add_newArcs (second :: rest) graph flow newGraph
        else
          add_newArcs (second :: rest) graph flow graph
  in add_newArcs path cloneGraph flow cloneGraph

(* Recupère le flot maximal d'un chemin *)
let rec max_flow_of_path_cap list graph acu = 
  match list with
  | [] -> 0
  | _ :: [] -> acu
  | first :: second :: rest -> 
    let arc = find_arc graph first second in
    match arc with 
    | None -> failwith "Arc not found"
    | Some a -> if a.lbl.cap < acu then max_flow_of_path_cap (second :: rest) graph a.lbl.cap else max_flow_of_path_cap (second :: rest) graph acu


(*trouver l'arc d'écart *)
let find_ecart_arc arc = { arc.lbl with flot_actuel = (arc.lbl.cap - arc.lbl.flot_actuel) }

(*trouver la graphe d'écart *)
let find_ecart_graph graph = 
  let newGraph = clone_nodes graph in 
    let find_un_ecart graph arc = 
      if arc.lbl.flot_actuel = 0 then 
        let newA = { src = arc.src ; tgt = arc.tgt ; lbl = arc.lbl.cap} in 
          new_arc graph newA
    else
      if arc.lbl.cap = arc.lbl.flot_actuel then  
          let newA2 = { src = arc.tgt ; tgt = arc.src ; lbl = arc.lbl.cap} in 
          new_arc graph newA2
      else(
          let l = find_ecart_arc arc in
          let newA3 = { src = arc.src ; tgt = arc.tgt ; lbl = l.flot_actuel} in 
          new_arc graph newA3
        (*  To do:
        let newA4 = { src = arc.tgt ; tgt = arc.src ; lbl = arc.lbl.flot_actuel} in 
           new_arc graph newA4
           *)
      )
  in 
  e_fold graph find_un_ecart newGraph


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
      match path with
      | None -> graph
      | Some p -> 
         let flot = max_flow_of_path_cap p graph 99 in 
         let newGraph = update_path_labels_flot p ecart_graph flot in 
         ford_fulkerson_algo_lblflot newGraph src tgt
 ;;

(* affiche le label flot*)
let lblflot_to_string label = Printf.sprintf "(%d/%d)" label.flot_actuel label.cap
 
(* transforme un string en label flot*) 
let string_to_label_flot label = Scanf.sscanf label "(%d/%d)" (fun flot c -> {flot_actuel = flot; cap = c})

(* transforme un label flot en string*)
let label_flot_to_string label = "(" ^ string_of_int label.flot_actuel ^ "/" ^ string_of_int label.cap ^ ")"

(* transforme en graph flot*)
let initial_graph_flot_from_string graph = gmap graph string_to_label_flot;;

(* exporte le flot_graph en string*)
let export_string_graph_from_flot_graph graph = gmap graph label_flot_to_string;;
