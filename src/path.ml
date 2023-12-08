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
