(*open Tools*)
open Graph


type path = id list

(* trouve un chemin possible sur le graphe*)
let rec find_path graph start finish visited =
  if start = finish then
    Some [start]
  else if List.mem start visited then
    None
  else
    let arcsSortant = out_arcs graph start in
    let rec loop = function
      | [] -> None
      | {tgt; _} :: rest ->
          match find_path graph tgt finish (start :: visited) with
          | Some p -> Some (start :: p)
          | None -> loop rest
    in
    loop arcsSortant

(* trouve le flot maximal d'un chemin du graph*)
let rec max_flow_of_path list graph acu = 
  match list with
  | [] -> 0
  | _ :: [] -> acu
  | first :: second :: rest -> 
    let arc = find_arc graph first second in
    match arc with 
    | None -> failwith "Arc not found"
    | Some a -> if a.lbl < acu then max_flow_of_path (second :: rest) graph a.lbl else max_flow_of_path (second :: rest) graph acu


(* Modifie les label du graph à partir du maxflow du chemin path
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
*)

(* let test_find_path_with_maxflow graphname src tgt = 
    let graph = from_file graphname in 
    let intgraph = gmap graph (fun x -> int_of_string x) in
    let path = find_path intgraph src tgt [] in
    match path with
    | None -> print_string "No path found" 
    | Some p -> print_int_list p; 
      let max = max_flow_of_path p intgraph 99 in print_string "Max flow : ";
      print_int max;;
  ;;
*) 