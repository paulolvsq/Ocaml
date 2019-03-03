type contenu = Mine | Numero of int 
type case = (contenu*bool)
type grille = case array array

let affiche_case c =
  let (contenu, bool) = c in
  match contenu, bool with
  | _, false -> print_string "-"
  | Mine, true -> print_string "X"
  | Numero x, true -> print_int x
			      
let affiche_grille grille =
  let h = Array.length grille - 1 in
  let l = Array.length grille.(0) - 1 in
  for x = 0 to h do
    for y = 0 to l do
      affiche_case (grille.(x).(y))
    done;
    print_newline();
  done

let grille_vide x y  =
   Array.make_matrix x y (Numero 0, false) 

let ajoute_mine (x, y) grille =
  match grille.(x).(y) with
  | Mine, _ -> false
  | _, _ -> grille.(x).(y)<-Mine, false ; true 

let random_pos grille =
  let x = Random.int (Array.length grille) - 1 in
  let y = Random.int (Array.length grille.(0)) - 1  in
  (x, y)

let genere_grille (x, y) h l =
  let nbmines = ((h*l)/10) in
  let g = grille_vide h l in
  g.(x).(y)<-Numero 0, true ;
  let  t = ref (ajoute_mine (random_pos g) g) in
  for i = 0 to nbmines do
    while  not !t do
      t:=ajoute_mine (random_pos g) g;
    done;
  done;
  g

let voisins (x, y) grille =
  let i =  Array.length grille - 1 in
  let j =  Array.length grille.(0) - 1 in 
  let liste = [(x+1,y);(x-1,y);(x,y+1);(x,y-1);(x+1,y+1);(x+1,y-1);(x-1,y+1);(x-1,y-1)] in
  List.filter (fun (x,y) -> x<=i  && y<=i && x<=j && y<=j) liste

let numerote_grille
