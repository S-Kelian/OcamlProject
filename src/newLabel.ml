type label_avec_flot = {
  actuel_flot : int;
  capacity : int;
}

let new_label = {actuel_flot = 0; capacity = 0}
(* label with capacity defined *)
let new_label_flot cap = {actuel_flot = 0; capacity = cap}



(* Methodes *)
  let compare_two_flot lbl1 lbl2 = lbl1.actuel_flot < lbl2.actuel_flot;;

  let divise_label lbl =  lbl with actuel_flot = (label.capacity - label.actuel_flot);;

  let string_label lbl =  "(" ^ string_of_int lbl.actuel_flot ^ "/" ^ string_of_int lbl.capacity ^ ")";;

  

  
