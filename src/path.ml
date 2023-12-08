open Graph

type path = id list

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

let rec max_flow_of_path list graph acu = 
  match list with
  | [] -> 0
  | _ :: [] -> acu
  | first :: second :: rest -> 
    let arc = find_arc graph first second in
    match arc with 
    | None -> failwith "Arc not found"
    | Some a -> if a.lbl < acu then max_flow_of_path (second :: rest) graph a.lbl else max_flow_of_path (second :: rest) graph acu
