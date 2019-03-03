open Niveau
open Adj
open Programme
open Graphics
(********************)
(* Dessin du niveau *)
(********************)
       
(* dimensions de la fenetre *)
let largeur = ref 0
let hauteur = ref 0

(*la fenetre est divis√© en deux parties :
- une partie haute pour dessiner la pile,
- une partie basse pour dessiner le terrain *)
let hauteur_haut = ref 0
let hauteur_bas = ref 0

(* nombre max d'instruction de la pile *)
let pilemax = 15

(* dimensions d'une case du niveau *)
let case_x = ref 0
let case_y = ref 0

(* bornes du niveau *)
let min_x = ref 0
let max_x = ref 0
let min_y = ref 0
let max_y = ref 0

(* fonction qui retourne les coordonn√©es du coin bas gauche
   d'une case de la grille, dans la fenetre graphique *)
let map_coord_to_graphics_coord (i,j) =
  (!case_x + ((i-(!min_x)) * !case_x)),
  (!hauteur_bas - (2*(!case_y) + ((j-(!min_y)) * !case_y)))

(* dessine la case de coordonn√©e c, avec la couleur pass√©e en parametre *)
let dessine_case (c:int*int) (couleur:couleur) =
  let i,j = c in 
  Format.printf "dessine case %d %d \n" i j ;
  let x,y = map_coord_to_graphics_coord c in
  begin
    match couleur with
    | Vert -> Graphics.set_color Graphics.green 
    | Rouge ->  Graphics.set_color Graphics.red
    | Bleu ->  Graphics.set_color Graphics.blue
    | Jaune ->  Graphics.set_color Graphics.yellow
    | None  ->  Graphics.set_color Graphics.white 
  end;
  Graphics.fill_rect x y !case_x !case_y; (*on dessine le rectangle formÈ par la case*)
  Graphics.set_color Graphics.black ; (*on fait les contours du dessin en noir pour le cas o˘ toutes les cases sont blanches*)
  Graphics.draw_rect x y !case_x !case_y
	      
				 
  
(* dessin d'une etoile dans la case 'c' *)
let dessine_etoile (c:int*int) =
  let i,j = c in

  Format.printf "dessine etoile %d %d \n" i j ; (*chaque variable reprÈsente un des 10 points de l'Ètoile*)
  let x,y = map_coord_to_graphics_coord c in
  let a = x+(!case_x*1/2), y+(!case_y*8/9) in
  let b = x+(!case_x*3/5), y+(!case_y*3/5) in
  let c = x+(!case_x*8/9), y+(!case_y*3/5) in
  let d = x+(!case_x*7/10), y+(!case_y*2/5) in
  let e = x+(!case_x*8/9), y+(!case_y*1/5) in
  let f = x+(!case_x*1/2), y+(!case_y*3/10) in
  let g = x+(!case_x*1/9), y+(!case_y*1/5) in
  let h = x+(!case_x*3/10), y+(!case_y*2/5) in
  let i = x+(!case_x*1/9), y+(!case_y*3/5) in
  let j = x+(!case_x*2/5), y+(!case_y*3/5) in
  let tableau = [|a;b;c;d;e;f;g;h;i;j|] in (*on remplit le tableau avec les coordonnÈes*)
  Graphics.set_color Graphics.yellow;
  Graphics.fill_poly tableau;
  
  Graphics.set_color Graphics.black;
  Graphics.draw_poly tableau;
  let distance_i_j_x =  (x+(!case_x*2/5)) -(x+(!case_x*1/5))  in (*on prend la premiËre longueur dont on a besoin pour dessiner les yeux entre le point i et j*)
  let distance_i_j_y =  (y+(!case_y*3/5)) - (y+(!case_y*3/5))  in (*on prend la deuxiËme longueur dont on a besoin pour dessiner les yeux entre le point i et j*)
  let distance_i_j = int_of_float (sqrt(float_of_int((distance_i_j_x*distance_i_j_x)+(distance_i_j_y*distance_i_j_y)))) in (*on applique la formule de la distance*)
  Graphics.fill_rect (x+(!case_x*2/5)+(distance_i_j)/2) (y+(!case_y*3/5)-(distance_i_j/2)) (distance_i_j/2) (distance_i_j/2) ; (*on dessine le rectangle des yeux avec les coordonnÈes des distances qu'on vient de calculer*)
  Graphics.fill_rect (x+(!case_x*2/5)-(distance_i_j)/4) (y+(!case_y*3/5)-(distance_i_j/2)) (distance_i_j/2) (distance_i_j/2) ; (*pareil*)
  Graphics.set_color Graphics.white ;
  Graphics.draw_rect (x+(!case_x*2/5)+(distance_i_j)/2) (y+(!case_y*3/5)-(distance_i_j/2)) (distance_i_j/2) (distance_i_j/2) ;	(*on dessine les contours maintenant*)	       
  Graphics.draw_rect (x+(!case_x*2/5)-(distance_i_j)/4) (y+(!case_y*3/5)-(distance_i_j/2)) (distance_i_j/2) (distance_i_j/2) 	(*pareil*)

(* dessine la liste poly avec les conteurs en blanc et la couleur interieur qui est "couleur"*)		     
let draw_fill poly couleur =
  Graphics.set_color couleur;
  Graphics.fill_poly poly;
  Graphics.set_color Graphics.white; (*on dessine les contours*)
  Graphics.draw_poly poly   

(* dessine le robot *)
let dessine_robot (t:etat_robot) : unit =
  let pos = t.pos in (*on rÈcupËre la position*)
  let i,j = pos in (*on part d'un point i et j*)
  Format.printf "dessine robot %d %d \n" i j ;
  let dir = t.dir in
  let x, y = map_coord_to_graphics_coord pos in  
  match pos,dir with
  | pos,Haut -> draw_fill [|(x+(!case_x/4),y+ !case_y/4)  ; (x+(!case_x/2),y + !case_y/2);   (x+(!case_x*3/4), y+(!case_y/4)); x+(!case_x/2),y+(!case_y*3)/4;(x+(!case_x/4),y+ !case_y/4)  |] Graphics.magenta	       
  | pos,Bas -> draw_fill [|  (x+(!case_x/4),y+(!case_y*3)/4); (x+(!case_x/2),y + !case_y/2); (x+(!case_x*3/4),y+(!case_y*3)/4) ;(x+(!case_x/2),y+(!case_y/4)); (x+(!case_x/4),y+(!case_y*3)/4) |] Graphics.magenta			 
  | pos,Gauche -> draw_fill [| (x+(!case_x*3/4),y+(!case_y*3)/4) ; (x+(!case_x/2),y + !case_y/2);(x+(!case_x*3/4),y+(!case_y/4)); (x+(!case_x/4),y+(!case_y/2));(x+(!case_x*3/4),y+(!case_y*3)/4) |] Graphics.magenta
  | pos,Droite -> fill_draw [| (x+(!case_x/4),y+(!case_y*3)/4) ; (x+(!case_x/2),y + !case_y/2);  (x+(!case_x/4),y+ !case_y/4) ; (x+(!case_x*3)/4,y+(!case_y/2)) ;(x+(!case_x/4),y+(!case_y*3)/4)  |] Graphics.magenta

				     
(* dessine le terrain, les etoiles et le robot *)
let dessine_niveau (map:niveau) : unit =
  let r = map.robot in
  let g = map.grille in 
  let liste_etoile = map.etoiles in
  List.iter (fun (x,elm) -> List.iter (fun (y,couleur) -> dessine_case (x,y) couleur ) elm) g ;
  List.iter dessine_etoile liste_etoile ;
  dessine_robot r 
  
  
(*********************)
(* dessin de la pile *)
(*********************)

(* fonction qui retourne les coordonn√©es du coin bas gauche
   d'une case de la pile, dans la fenetre graphique *)
let coord_case (i:int) : (int*int) =
  let largeur_case = (!largeur) / (pilemax + 3) in
  (largeur_case * (i+1)),  !hauteur_bas + largeur_case*2

(* dessine la i-eme case de la pile *)
let dessine_case_pile (col:couleur) (i:int) : unit =
  let x,y = coord_case i in
  let largeur_case = (!largeur) / (pilemax + 3) in
  begin
    match col with
    | Vert -> Graphics.set_color Graphics.green 
    | Rouge ->  Graphics.set_color Graphics.red
    | Bleu ->  Graphics.set_color Graphics.blue
    | Jaune ->  Graphics.set_color Graphics.yellow
    | None  ->  Graphics.set_color Graphics.white 
  end;
  Graphics.fill_rect x y largeur_case largeur_case;
  Graphics.set_color Graphics.black ;
  Graphics.draw_rect x y largeur_case largeur_case

		     
(* dessine la i-eme commande de la pile *)
let dessine_commande (i:int) ((col,e):Programme.commande) : unit =

  dessine_case_pile col i ;
  let x,y = coord_case i in
  let largeur_case = (!largeur) / (pilemax + 3) in
  Graphics.moveto (x+largeur_case/2) (y+largeur_case/2);
  match e with
  | Avancer ->  Graphics.draw_string "->" 
  | RotGauche -> Graphics.draw_string "-^" 
  | RotDroite -> Graphics.draw_string "-v" 
  | Colorie (c) ->
     begin
       match c with
       | Vert -> Graphics.draw_string "v"
       | Rouge -> Graphics.draw_string "r"
       | Bleu -> Graphics.draw_string "b"
       | Jaune -> Graphics.draw_string "j"
       | None -> Graphics.draw_string ""
     end
  | Appel (s) -> Graphics.draw_string s 
				      
(* affichage de la pile dans la partie sup√©rieure de l'√©cran *)
let rec dessine_pile (pile:Programme.sequence) : unit =

  let rec loop pile acc =

      match pile with
      | [] -> ()
      | h::t -> dessine_commande acc h;
		loop t (acc+1)

  in
  loop pile 0
		     
(*****************)
(* fin de partie *)
(*****************)

let font() =
  Graphics.set_font "*--0-0-*iso8859-*"

(* affiche la chaine "Defaite !" au centre de l'√©cran *)
let perdu () : unit =
  let l = !largeur /2 in
  let h = !hauteur /2 in
  Graphics.moveto l h ;
  Graphics.clear_graph();
  Graphics.draw_string ("Defaite ! ")

(* affiche la chaine "Victoire ! " au centre de l'√©cran *)
let gagne (x) : unit =
  let l = !largeur /2 in
  let h = !hauteur /2 in
  Graphics.moveto l h ;
  Graphics.clear_graph();
  Graphics.draw_string ("Victoire ! en "^(string_of_int x)^" iterations !" )

(*******************************************************)
(* Cr√©ation et initialisation de l'interface graphique *)
(*******************************************************)

(* efface l'√©cran *)
let clear () : unit =
  Graphics.clear_graph()

(* initialisation *)
let init map (x,y) : unit =
  Graphics.open_graph (" "^(string_of_int x)^"x"^(string_of_int y));
  font();
  let (minx, maxx),(miny, maxy) = bornes map.grille in
  min_x := minx;
  max_x := maxx;
  min_y := miny;
  max_y := maxy;
  largeur := x;
  hauteur := y;
  hauteur_haut :=  (y * 25) / 100 ;  (* !hauteur/4 * y; *) 
  hauteur_bas := !hauteur - !hauteur_haut ;(*3* !hauteur/4 * y;*)
  case_x := x/(!max_x - !min_x +3);
  case_y := !hauteur_bas /(!max_y - !min_y +3) ;

  Format.printf "largeur %i hauteur  %i hauteur_haut %i hauteur_bas %i case_x %i case_y %d minx %d maxx %d miny %d maxy %d  \n" !largeur !hauteur !hauteur_haut !hauteur_bas !case_x !case_y !min_x !max_x !min_y !max_y 	   
