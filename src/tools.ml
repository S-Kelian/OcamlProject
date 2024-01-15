open Graph

(* clone tous les sommets d'un graphe*)
let clone_nodes (gr: 'a graph) =
  n_fold gr (fun g id -> new_node g id) empty_graph

(* applique une fonction f à tous les arcs d'un graphe *)
let gmap gr f =
  let newGraph = clone_nodes gr in
  e_fold gr (fun g { src = n1; tgt = n2; lbl = label } ->
    new_arc g { src = n1; tgt = n2; lbl = f label }
  ) newGraph

(* ajoute un arc de valeur n entre id1 et id2*)
let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | None -> new_arc g { src = id1; tgt = id2; lbl = n }
  | Some x -> new_arc g { src = id1; tgt = id2; lbl = x.lbl + n }
