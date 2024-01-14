

(*open Path*)
open Tools
open Graph

type lbl_flot = {
  flot_actuel : int;
  cap : int;
}

let get_lbl label = label.lbl

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

  (*let rec max_flow_of_path_cap list graph acu = 
    match list with
    | [] -> 0
    | _ :: [] -> acu
    | first :: second :: rest -> 
      let arc = find_arc graph first second in
      match arc with 
      | None -> failwith "Arc not found"
      | Some a -> if a.lbl.cap < acu then max_flow_of_path_cap (second :: rest) graph a.lbl.cap else max_flow_of_path_cap (second :: rest) graph acu
*)
  (*trouver l'arc d'écart *)

let find_ecart_arc arc =
  { arc with lbl = { arc.lbl with flot_actuel = arc.lbl.cap - arc.lbl.flot_actuel } }

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
          let n = find_ecart_arc arc in
          let newA3 = { src = arc.src ; tgt = arc.tgt ; lbl = n.lbl.flot_actuel} in 
          new_arc graph newA3
        (*  To do:
        let newA4 = { src = arc.tgt ; tgt = arc.src ; lbl = arc.lbl.flot_actuel} in 
           new_arc graph newA4
           *)
      )
  in 
  e_fold graph find_un_ecart newGraph








  

  


  
