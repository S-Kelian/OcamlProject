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

let find_arc_in_path graph path id1 id2 =
  let rec loop = function
    | [] -> None
    | _ :: [] -> None
    | first :: second :: rest -> 
      if first = id1 && second = id2 then
        find_arc graph first second
      else
        loop (second :: rest)
  in
  loop path

(* trouve le flot minimal d'un chemin du graph*)
let rec min_flow_of_path list graph acu = 
  match list with
  | [] -> 0
  | _ :: [] -> acu
  | first :: second :: rest -> 
    let arc = find_arc graph first second in
    match arc with 
    | None -> failwith "Arc not found"
    | Some a -> if a.lbl < acu then min_flow_of_path (second :: rest) graph a.lbl else min_flow_of_path (second :: rest) graph acu
