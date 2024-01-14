open Graph

let clone_nodes (gr: 'a graph) =
  n_fold gr (fun g id -> new_node g id) empty_graph

let gmap gr f =
  let newGraph = clone_nodes gr in
  e_fold gr (fun g { src = n1; tgt = n2; lbl = label } ->
    new_arc g { src = n1; tgt = n2; lbl = f label }
  ) newGraph

let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | None -> new_arc g { src = id1; tgt = id2; lbl = n }
  | Some x -> new_arc g { src = id1; tgt = id2; lbl = x.lbl + n }
