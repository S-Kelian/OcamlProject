(*open Tools*)
open Graph


type path = id list

(* trouve un chemin possible sur le graphe*)
let rec find_path graph start finish visited =
  (*On vérifie que le start ne soit pas égal au finish sinon on retourne juste l'id correspondant*)
  if start = finish then
    Some [start]
  (* On vérifie qu'on n'est pas passé par ce sommet*)
  else if List.mem start visited then
    None
  else
    (* On récupère les arcs sortant du sommet*)
    let arcsSortant = out_arcs graph start in
    (* On parcourt les arcs sortant du sommet*)
    let rec loop = function
      | [] -> None
      | {tgt; _} :: rest ->
          let arc = find_arc graph start tgt in
          match arc with
          | None -> failwith "Arc not found"
          (* Si le flot de l'arc est nul on passe à l'arc suivant*)
          | Some a -> if a.lbl = 0 then loop rest else
              match find_path graph tgt finish (start :: visited) with
              | Some p -> Some (start :: p)
              | None -> loop rest
    in
    loop arcsSortant

(* vérifie que l'arc entre id1 et id2 est bien dans le chemin et retourne cet arc*)
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

(* trouve le flot minimal d'un chemin du graph (la plus petite valeur des arcs)*)
let rec min_flow_of_path list graph acu = 
  match list with
  | [] -> 0
  | _ :: [] -> acu
  | first :: second :: rest -> 
    let arc = find_arc graph first second in
    match arc with 
    | None -> failwith "Arc not found"
    | Some a -> if a.lbl < acu then min_flow_of_path (second :: rest) graph a.lbl else min_flow_of_path (second :: rest) graph acu

(* affiche la liste des sommets qui passe dans le chemin (fonction de test)*)
let rec print_path list = 
  match list with
  | [] -> ()
  | last :: [] -> Printf.printf "%d\n" last
  | first :: second :: rest -> 
    Printf.printf "%d -> " first ;
    print_path (second :: rest)
;