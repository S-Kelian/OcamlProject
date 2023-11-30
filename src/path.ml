open Graph

let rec find_path graph start finish path =
  if start = finish then
    (List.rev (start :: path))
  else
    let arcsSortant = out_arcs graph start in
    let rec loop arcsSortant =
      match arcsSortant with
      | [] -> []
      | {src; tgt; lbl} :: rest ->
      ;;