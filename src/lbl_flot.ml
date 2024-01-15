open Tools
open Graph
open Path

type lbl_flot = {
  flot_actuel : int;
  cap : int;
}

let string_lbl_flot lbl = "(" ^ string_of_int lbl.flot_actuel ^ "/" ^ string_of_int lbl.cap ^ ")";;
let lblflot_to_string (label:lbl_flot) = "(" ^ string_of_int label.flot_actuel ^ "/" ^ string_of_int label.cap ^ ")"
let string_to_lblflot label = Scanf.sscanf label "%s" (fun cap -> if cap = "∞" then {flot_actuel = 0; cap = max_int} else
   {flot_actuel = 0; cap = int_of_string cap})

let update_flot graph path flot =
  let newGraph = clone_nodes graph in
  e_fold graph (fun acu arc -> 
    let arc_found = find_arc_in_path graph path arc.src arc.tgt in
    match arc_found with
    | None -> new_arc acu {src = arc.src; tgt = arc.tgt; lbl = arc.lbl}
    | Some _ -> new_arc acu {src = arc.src; tgt = arc.tgt; lbl = {flot_actuel = arc.lbl.flot_actuel + flot; cap = arc.lbl.cap}}
  ) newGraph 

  (*trouver l'arc d'écart *)
let find_ecart_arc arc =
  { arc with lbl = { arc.lbl with flot_actuel = arc.lbl.cap - arc.lbl.flot_actuel } }

(*trouver la graphe d'écart *)
let find_ecart_graph graph = 
  let newGraph = clone_nodes graph in 
    let find_un_ecart graph arc = 
      (* Toute la capacité de l'arête est disponible *)
      if arc.lbl.flot_actuel = 0 then 
        new_arc graph { src = arc.src ; tgt = arc.tgt ; lbl = arc.lbl.cap}
      else
        if arc.lbl.cap = arc.lbl.flot_actuel then  
          (* Toute la capacité de l'arête est indisponible *)
          new_arc graph { src = arc.tgt ; tgt = arc.src ; lbl = arc.lbl.cap} 
        else
          (* Redessiner les deux arcs *)
          let n = find_ecart_arc arc in
          let g = new_arc graph { src = arc.src ; tgt = arc.tgt ; lbl = n.lbl.flot_actuel}
          in  
          new_arc g { src = arc.tgt ; tgt = arc.src ; lbl = arc.lbl.flot_actuel}
  in 
  e_fold graph find_un_ecart newGraph
