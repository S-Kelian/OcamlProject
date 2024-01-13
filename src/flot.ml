
open Graph
open Gfile
open Tools

type lbl_flot = {
  flot_actuel : int;
  cap : int;
}

let string_lbl_flot lbl = "(" ^ string_of_int lbl.flot_actuel ^ "/" ^ string_of_int lbl.cap ^ ")";;

type label_flot_cost = {
  current_flot: int;
  capacity: int;
  cost: int
}


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
 
  let find_ecart_arc arc = { arc.lbl with flot_actuel = (arc.lbl.cap - arc.lbl.flot_actuel) }

  
