
(* open Gfile*)
open Graph

let clone_nodes (gr:'a graph) = n_fold gr (fun g id -> new_node g id) empty_graph;;
let gmap gr f = 
let newGraph = clone_nodes gr in
  e_fold gr (fun g {src=n1; tgt=n2; lbl=label} -> new_arc g {src=n1; tgt=n2; lbl=f label}) newGraph;;

(* add value of label*)
let add_arc g id1 id2 n= 
  match find_arc g id1 id2 with
  | None -> new_arc g {src=id1;  tgt=id2; lbl=n}
  | Some x -> new_arc g {src=id1;  tgt=id2; lbl=x.lbl+n};;

(* affiche un chemin (liste des id des sommets) 
let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;;
  *)

(* test pour la fonction find_path et max_flow_of_path*)





