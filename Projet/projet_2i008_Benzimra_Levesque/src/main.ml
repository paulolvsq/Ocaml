open Programme
open Niveau

let loop (map:niveau) (pgm:programme) : unit =
  let rec speed map pile acc =
    Vue.clear();
    Vue.dessine_niveau map;
    Vue.dessine_pile pile;
    match map.etoiles with
    | [] ->
       Vue.gagne(acc);
       ignore (read_line ())
    | _ ->
       let n,s = une_etape pgm map (pile) in
       let sleep x =
	 Unix.system("sleep "^(string_of_float x))
       in
       sleep 0.05;
       speed n s (acc+1)
  in
  
  let rec aux map pile acc =   
    Vue.dessine_niveau map;
    Vue.dessine_pile pile;
    match map.etoiles with
    | [] ->
       Vue.gagne(acc);
       ignore (read_line ())
    | _ ->
       let n,s = une_etape pgm map (pile) in
       let status = Graphics.wait_next_event [Graphics.Key_pressed] in
       match status.key with
       | 'r' ->	 speed n s (acc+1)
       | _ -> aux n s (acc+1)

  in

       try
    aux map (pile_initiale pgm) 0
  with Tomber ->  Vue.perdu();
		 ignore (read_line ())

let _ =
  (* utilitaire pour vérifier qu'une string est suffixe d'une autre *)
  let string_ends_with string suffix =
    let open String in
    let la = length string
    and lb = length suffix in
    lb <= la && sub string (la - lb) lb = suffix
  in
  (* on vérifie qu'on a passé le bon nombre d'argument *)
  if Array.length Sys.argv <> 3 then
    failwith "robot a besoin d'une map et d'un programme!"
  else
    begin
      let niveau,prog =
        if string_ends_with Sys.argv.(1) ".map" then Sys.argv.(1),Sys.argv.(2)
        else Sys.argv.(2),Sys.argv.(1)
      in
      let pgm = Parsing.parse_prog prog in
      let map = Parsing.parse_niveau niveau in
      Programme.verifie pgm map;
      Vue.init map (800,800);
      loop map pgm
    end
