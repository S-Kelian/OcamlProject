open Graph
open Gfile
open Path

let clone_nodes (gr:'a graph) = n_fold gr (fun g id -> new_node g id) empty_graph;;
let gmap gr f = 
  let newGraph = clone_nodes gr in
  e_fold gr (fun g {src=n1; tgt=n2; lbl=label} -> new_arc g {src=n1; tgt=n2; lbl=f label}) newGraph;;

let add_arc g id1 id2 n= 
  match find_arc g id1 id2 with
  | None -> new_arc g {src=id1;  tgt=id2; lbl=n}
  | Some x -> new_arc g {src=id1;  tgt=id2; lbl=x.lbl+n};;


let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;;

let test_find_path_with_maxflow graphname src tgt = 
  let graph = from_file graphname in 
  let intgraph = gmap graph (fun x -> int_of_string x) in
  let path = find_path intgraph src tgt [] in
  match path with
  | None -> print_string "No path found" 
  | Some p -> print_int_list p; 
    let max = max_flow_of_path p intgraph 99 in print_string "Max flow : ";
    print_int max;;
;;

