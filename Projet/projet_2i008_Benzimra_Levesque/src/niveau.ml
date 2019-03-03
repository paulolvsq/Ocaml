open Adj 
(*********)
(* Types *)
(*********)

(* on se limite à quatre couleurs, cela facilitera le parsing des maps *)
(* une case est soit colorée, soit transparente (None) *)
type couleur = Vert
             | Rouge
             | Bleu
             | Jaune
             | None

(* l'orientation défini la direction vers laquelle le robot avancera *)
type orientation = Haut
                 | Bas
                 | Gauche
                 | Droite

let rot_gauche (orientation:orientation) : orientation =
  match orientation with
  | Haut -> Gauche
  | Bas -> Droite
  | Droite -> Haut
  | Gauche -> Bas

let rot_droite (orientation:orientation) : orientation =
  match orientation with
  | Haut -> Droite
  | Bas -> Gauche
  | Droite -> Bas
  | Gauche -> Haut

type coordonnee = int * int

(* un etat du robot est défini par des coordonnées et une orientation *)
type etat_robot = {
  pos : coordonnee;
  dir : orientation;
  }

let avancer (robot : etat_robot) : etat_robot =
  let pos = robot.pos in
  let dir = robot.dir in
  
  match dir,pos with
  | Haut,(x,y) -> let nrobot = {pos = x,y-1;dir=robot.dir }in
				nrobot
  | Bas,(x,y) -> let nrobot = {pos = x,y+1;dir=robot.dir} in
		 nrobot
  | Droite,(x,y) -> let nrobot = {pos = x+1,y;dir=robot.dir} in
		 nrobot
  | Gauche,(x,y) -> let nrobot = {pos = x-1,y;dir=robot.dir} in
		 nrobot

(* un niveau est constituée d'un robot et
   d'une liste d'adjascence de liste d'adjascence de cases *)
type niveau = {
  grille  : couleur Adj.matrix;
  robot     : etat_robot;
  (* une fonction a un nom et un nombre max d'instructions autorisées *)
  fonctions : (string * int) list;
  etoiles   : (int*int) list;
  }

let get_couleur (c : coordonnee)  (etat : niveau) : couleur =
  get_matrix c etat.grille

let set_couleur (c : coordonnee) (e : couleur) (etat : niveau) : niveau =
  let ngrille = set_matrix c e etat.grille in
  let netat = {grille = ngrille;
	       robot = etat.robot;
	       fonctions = etat.fonctions;
	       etoiles = etat.etoiles} in
  netat

    
let robot_avancer (etat : niveau) : niveau =
  let npos = avancer etat.robot in
  let netat = {grille=etat.grille;
	       robot = npos;
	       fonctions = etat.fonctions;
	       etoiles = etat.etoiles} in
  netat

let robot_gauche (etat : niveau) : niveau =
  let orientation = etat.robot.dir in
  let newori= rot_gauche orientation in
  let nrobot = { pos = etat.robot.pos; dir = newori };in
  let netat = {grille = etat.grille;
	      robot = nrobot;
	      fonctions = etat.fonctions;
	      etoiles = etat.etoiles} in
  netat
    
					    

let robot_droite (etat : niveau) : niveau =
 let orientation = etat.robot.dir in
  let newori= rot_droite orientation in
  let nrobot = { pos = etat.robot.pos; dir = newori };in
  let netat = {grille = etat.grille;
	      robot = nrobot;
	      fonctions = etat.fonctions;
	      etoiles = etat.etoiles} in
  netat
    

let robot_colorie (couleur:couleur) (etat: niveau) : niveau =
  set_couleur etat.robot.pos couleur etat

let enleve_etoile etat : niveau =
  let rec loop liste acc  =
    match liste with
    |[] -> acc
    | h::t -> if h = etat.robot.pos then
		loop t acc
	      else
		loop t (h::acc)
  in
  {etat with etoiles = (loop etat.etoiles [] )} 

let test_couleur (co : couleur) (etat : niveau) : bool =
  let pos = etat.robot.pos in
  
  match co with
  | None -> true
  | co  ->   get_matrix pos etat.grille = co 

let case_valide (etat : niveau) : bool =
  let pos = etat.robot.pos in
  mem_matrix pos etat.grille 
