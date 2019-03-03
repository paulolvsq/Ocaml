let rec occurence char freqs =
  match char, freqs with
  | _, [] -> [(char, 1)]
  | y, (h,x)::t -> if h = y then (h, x+1)::t
		   else (h, x)::occurence char t
					  
let est_minuscule char =
  int_of_char char <= 122 && int_of_char char >= 97

let input_char_opt i =
  try
    let c = input_char i in
    Some c
  with
  | End_of_file -> None

let frequences_fichier i =
  let rec loop i acc =
    match (input_char_opt i) with
    | Some c -> if est_minuscule c then loop i (occurence c acc) else loop i acc 
    | None -> acc
  in
  loop i []

let  devine_decalage liste =
  match liste with
  | [] -> failwith "liste vide"
  | (c, f)::t ->
     begin
       let (i,j) =
	 List.fold_left( fun (a,b) (c,f)->
			 if b<f
			 then (c,f)
			 else (a,b))
		       (c,f)
		       t
       in
       int_of_char i - int_of_char 'e'
     end

let dechiffre_char d ch =
  char_of_int((((int_of_char ch - int_of_char 'a')- d)+26) mod 26 + int_of_char 'a')

let main =
  let fileIn = open_in(Sys.argv.(1)) in
  let listeLettres = frequences_fichier fileIn in
  let decalage = devine_decalage listeLettres in
  let file2In = open_in(Sys.argv.(1)) in
  let fileOut = open_out("res") in
  let rec ecrire fluxin fluxout d =
    match (input_char_opt fluxin) with
    | None -> ()
    | Some c -> if est_minuscule c
		then (output_char fluxout (dechiffre_char d c);ecrire fluxin fluxout d)
		else (output_char fluxout c;ecrire fluxin fluxout d)
  in
  ecrire file2In fileOut decalage
