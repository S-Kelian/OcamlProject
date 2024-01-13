
open Graph
open Gfile
open Tools

type lbl_flot = {
  flot_actuel : int;
  cap : int;
}

let string_lbl_flot lbl = "(" ^ string_of_int lbl.flot_actuel ^ "/" ^ string_of_int lbl.cap ^ ")";;



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

  let rec max_flow_of_path_cap list graph acu = 
    match list with
    | [] -> 0
    | _ :: [] -> acu
    | first :: second :: rest -> 
      let arc = find_arc graph first second in
      match arc with 
      | None -> failwith "Arc not found"
      | Some a -> if a.lbl.cap < acu then max_flow_of_path_cap (second :: rest) graph a.lbl.cap else max_flow_of_path_cap (second :: rest) graph acu


  let find_ecart_arc arc = { arc.lbl with flot_actuel = (arc.lbl.cap - arc.lbl.flot_actuel) }


  
