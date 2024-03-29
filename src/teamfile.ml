open Graph
open Printf

(************Nouvelles structures et getters************)
type team = {
    id : int;
    nom : string;
    gagne : int;
    pertes : int;
    matchs_restants_total : int;
}

type game = {
    monteam : string;
    competitor : string;
    nb_matchs_restants : int;
}

type name = string

(*Obtenir le nombre de matches restants entre deux équipes données *)
let get_nb_match_game ((monteam:name),(competitor:name)) (games:game list) =
  match List.find_opt (fun game -> (game.monteam = monteam && game.competitor = competitor) || (game.monteam = competitor && game.competitor = monteam)) games with
  | Some match_info -> match_info.nb_matchs_restants
  | None -> 0 

(*Obtenir le nombre de matchs restants pour une équipe *)
let get_list_nb_match_total_two_teams (nom:name) (teams:team list) =
  match List.find_opt (fun t -> (t.nom= nom)) teams with
  | Some match_info -> match_info.matchs_restants_total
  | None -> 0 

(* Obtenir le nombre de matchs gagnés par une équipe *)
  let get_gagne (nom:name) (teams:team list) =
    match List.find_opt (fun t -> (t.nom= nom)) teams with
    | Some match_info -> match_info.gagne
    | None -> 0 
  

(************ Lire les fichiers *************)

(* Lit une ligne avec une équipe. *)

let read_team line listTeam listGame =
  try Scanf.sscanf line "t %d %s %d %d %d" (
   fun idteam nomteam nbwin nbloses nbleft -> (
     let new_team = {id = idteam; nom = nomteam; gagne = nbwin; pertes = nbloses; matchs_restants_total = nbleft} in
     (new_team :: listTeam, listGame)
   )
  )
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(* Lit une ligne avec le nombre de matchs restants des deux équipese. *)
let read_game line listTeam listGame  =
  try Scanf.sscanf line "g %s %s %d"
    (fun nomteam anotherteam nbleft ->
      let new_game = {monteam = nomteam; competitor = anotherteam; nb_matchs_restants = nbleft} in
      (listTeam, new_game :: listGame)
    ) 
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"


(* Lit une commentaire. *)
let read_comment_team line listTeam listGame =
  try Scanf.sscanf line " %%" (listTeam,listGame) 
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line;
    failwith "from_file"




(************ string <-> pair  ***********)

(*Convertir le string MI_DC en pair(MI,DC)*)
let split_teams team_string =
  match String.split_on_char '_' team_string with
  | [team1; team2] -> (team1, team2)
  | _ -> failwith "Invalid team string format"

(*Convertir le pair(MI,DC) en string MI-DC *)
let string_two_teams_nom lstPairs =
  List.map (fun (team1, team2) -> team1 ^ "_" ^ team2 ) lstPairs


(*Trouver l'index de x dans la liste*)
let rec find_index_of_x x lst =
  match lst with
  | [] -> -1  (* Element not found, return -1 *)
  | hd :: tl ->
    if hd = x then 0  (* Element found at index 0 *)
    else
      let subresult = find_index_of_x x tl in
      if subresult = -1 then failwith "x not found in list"  
      else 1 + subresult  
    
let node_id_to_string id listT3 listG3 =
  match id with
  | 0 -> "S"
  | x when x > 0 && x <= List.length listG3 -> List.nth listG3 (x - 1)
  | x when x > List.length listG3 && x <= (List.length listG3 + List.length listT3) ->
    List.nth listT3 (x - 1 - List.length listG3)
  | x when x > (List.length listG3 + List.length listT3) && x <= (List.length listG3 + List.length listT3 + 1) -> "T"
  | _ -> failwith "ID Node Not Exists"
;;

(* Obtenez l'ID basé sur la nom du nœud *)   
let node_string_to_id listT3 listG3 nomNode =
  match nomNode with
  | "S" -> 0
  | "T" -> List.length listG3 + List.length listT3 + 1
  | x when List.mem x listG3 -> find_index_of_x x listG3 + 1
  | x when List.mem x listT3 -> find_index_of_x x listT3 + 1 + List.length listG3 
  | _ -> failwith "Node Name Not Exists"
;;


(************ Create Graph methodes ***********)

(* Obtenez tous les noms des équipes sauf celle qui a été désignée *)
let node_team_nom listT team = List.filter_map (fun oneteam -> if oneteam.nom <> team then Some oneteam.nom else None) listT
(* Obtenez toutes les équipes sauf celle qui a été désignée *)
let node_team listT team = List.filter_map (fun oneteam -> if oneteam.nom <> team then Some oneteam else None) listT


(* Obtenir tous les nœuds du jeu *)
let node_game listG team = List.filter_map (fun onegame -> if (onegame.monteam <> team) && (onegame.competitor <> team) then Some onegame else None) listG
let node_game_nom_pair listG team = List.filter_map (fun onegame -> if (onegame.monteam <> team) && (onegame.competitor <> team) 
  then Some (onegame.monteam ,onegame.competitor ) else None) listG 

(*Supprimez les doublons *)
let remove_reverse_combinations lst =
  let rec aux acc = function
    | [] -> acc
    | (x, y) :: tl ->
      if List.mem (y, x) tl || List.mem (x, y) tl then
        aux acc tl
      else
        aux ((x, y) :: acc) tl
  in
  aux [] lst 

(* Obtenir tous les noms des nœuds du jeu, à l'exception de l'équipe spécifiée et des doublons *)
let node_game_nom lst t = string_two_teams_nom (remove_reverse_combinations (node_game_nom_pair lst t))



(************ Create Graph ***********)

let construct_graph (listT4,listG4) nomt = 

let listT3 = node_team_nom listT4 nomt in 
let listG3 = node_game_nom listG4 nomt in 

let listT3_complete = node_team listT4 nomt in
let listG3_complete = node_game listG4 nomt in


let gr =  new_node empty_graph 0  in 
let idt = node_string_to_id listT3 listG3 "T" in 
(*graph avec source et puits*)
let gr_s_t = new_node gr idt in                                     
            
(* Ajouter les nœuds du jeu et l'arc entre celui-ci et la source*)
let rec construct_g1 listT listG graph_source = 
    match listG with
    | [] -> graph_source 
    | x::xrest ->
    let graph1 = new_node graph_source (node_string_to_id listT3 listG3 x) in 
    let l = get_nb_match_game (split_teams x) listG3_complete in
    let lblstr = string_of_int l in
    let graph1_arc = new_arc graph1 { src = 0 ; tgt = node_string_to_id listT3 listG3 x ; lbl = lblstr }
    in 
    construct_g1 listT xrest graph1_arc

in 
let g1 = construct_g1 listT3 listG3 gr_s_t in 

(* Ajouter les nœuds de l'équipe*)
let rec construct_g2 listT listG graph_source = 
    match listT with
    | [] -> graph_source 
    | x::xrest ->
    let graph2 = new_node graph_source (node_string_to_id listT3 listG3 x) 
    in 
    construct_g2 xrest listG graph2

in 
let g2 = construct_g2 listT3 listG3 g1 in  

(* Ajouter l'arc entre les nœuds de l'équipe et le nœud de jeu correspondant *)
let rec construct_g2_2 listT listG gr = 
  match listG with
  | [] -> gr 
  | g::grest ->
    let (team1,team2) = split_teams g in 
    let inf = "∞" in
    let graph2_2_arc1 = new_arc gr  { src = node_string_to_id listT3 listG3 g ; tgt = node_string_to_id listT3 listG3 team1 ; lbl= inf } in 
    let graph2_2_arc2 = new_arc graph2_2_arc1  { src = node_string_to_id listT3 listG3 g ; tgt = node_string_to_id listT3 listG3 team2 ; lbl= inf }
       in 
    construct_g2_2 listT grest graph2_2_arc2

in 
let g2_2 = construct_g2_2 listT3 listG3 g2 in 
 
(* Ajouter l'arc entre les nœuds de l'équipe et le puits *)
let rec construct_g3 listT listG graph2 = 
  match listT with
    | [] -> graph2
    | tt::ttrest ->
    let idt = node_string_to_id listT3 listG3 "T" in 
    let gagnemont = get_gagne nomt listT4 in
    let gagne = get_gagne tt listT3_complete in
    let restant = get_list_nb_match_total_two_teams nomt listT4 in
    let res = (gagnemont + restant - gagne )in 
    let nbstr = string_of_int res in
    let graph_final = new_arc graph2 { src = node_string_to_id listT3 listG3 tt ; tgt = idt  ; lbl = nbstr }
  in 
  construct_g3 ttrest listG graph_final

in  
  construct_g3 listT3 listG3 g2_2


  (* let constrct graph lt lg nomteam = construct_g3 listT3 listG3 g3 *)
 
;;

(* Calcule la somme des valeurs des arcs sortants de la source de notre graphe*)
let sum_of_source graph = 
  let arcs = out_arcs graph 0 in
  let rec loop arcs acu
    = match arcs with
      | [] -> acu
      | arc::arcs -> loop arcs (acu + int_of_string arc.lbl)
  in loop arcs 0
;;


(******* Infile *******)
(* Retourner le graph avec une pair de teamlist et gamelist*)

let from_file_game path team =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop listTeam listGame =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (listT,listG) =
        (* Ignore empty lines *)
        if line = "" then (listTeam,listGame)

        (* The first character of a line determines its content : t or g. *)
        else match line.[0] with 
          | 't' -> read_team line listTeam listGame 
          
          | 'g' -> read_game line listTeam listGame

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment_team line listTeam listGame
      in      
      loop listT listG

    with End_of_file -> (listTeam,listGame)
  in

  let (listTeam,listGame) = loop []  [] in
  let final_graph = construct_graph (listTeam,listGame) team in

  close_in infile ;
  (final_graph,(listTeam,listGame))
;;



(****** Outfile ************)


  let export_game path graph (listT4,listG4)=

  let ff = open_out path in
  fprintf ff "digraph our_graph {\n" ;
  fprintf ff "  fontname=\"Helvetica,Arial,sans-serif\"\n" ;
  fprintf ff "  node [fontname=\"Helvetica,Arial,sans-serif\"]\n" ;
  fprintf ff "  edge [fontname=\"Helvetica,Arial,sans-serif\"]\n" ;
  fprintf ff "  rankdir=LR;\n" ;
  fprintf ff "  node [shape = circle];\n";
  
  (* Write all arcs *)
  e_iter graph (fun arc -> fprintf ff "  %s -> %s [label = \"%s\" ];\n" (node_id_to_string arc.src listT4 listG4) (node_id_to_string arc.tgt listT4 listG4) arc.lbl) ;
  fprintf ff "}" ;

  close_out ff ;
  ()
