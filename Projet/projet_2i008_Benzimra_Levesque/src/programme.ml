(**********************)
(* Types et printers  *)
(**********************)
open Niveau
open Adj
(* Les actions possibles du robot *)
type action = Avancer
            | RotGauche
            | RotDroite
            | Colorie of Niveau.couleur
            | Appel   of string

			   
(* une commande est un coupe couleur, action : L'action ne se déclence
   que si le robot est une sur case de la couleur associée *)
type commande = Niveau.couleur * action

(* une liste de commande est appellée une séquence *)
type sequence = commande list

(* une fonction a un nom et une liste de commande a effectuer
   lorsqu'elle est appellée *)
type fonction = string * sequence

(* un programme est une liste de fonctions *)
type programme = fonction list

(***********)
(* Actions *)
(***********)

exception Tomber
exception PileVide

(* valeur initiale de la pile d'appel *)
let pile_initiale (prog : programme) : sequence =
  match prog with
  | [] -> []
  | (nom,sequence)::suiteProg -> [(None,Appel(nom))]

                  
(* retourne la liste de commande associée à une fonction *)
let trouve_fonction (s : string) (prog : programme) =
  let rec loop prog  =
     match prog with
     | [] -> []
     | (nom,sequence)::suiteProg -> if nom  = s then
				      sequence
				    else loop suiteProg
  in
  loop prog 

(* verifie si la partie est terminé *)
let est_fini etat =
  etat.etoiles = []



(* effectue une seule etape d'un programme *)
let une_etape (prog:programme) (etat:niveau) (pile:sequence) : niveau * sequence =
  let n =  enleve_etoile etat in
  if case_valide n then 
    match pile with
    | [] -> raise PileVide
    | (couleur,action)::suite -> if test_couleur couleur n then
				   begin 
				     match action with
				     | Avancer ->  (robot_avancer n),suite
				     | RotGauche -> (robot_gauche n),suite
				     | RotDroite -> (robot_droite n),suite
				     | Colorie (c) -> (robot_colorie c n),suite 
				     | Appel (s) -> n,(trouve_fonction s prog)@suite 
				   end
				 else
				   n,suite
  else 
    raise Tomber

	  
(* verifie qu'un niveau est valide et qu'un programme lui est conforme *)
let verifie (p:programme) (n:niveau) : unit = 


  let fonction2 f =
    List.fold_left (fun  acc (string,int) ->if List.length (trouve_fonction string p) <= int then
					      acc = true
					    else failwith "erreur trop de fonction") true f in
  
  let etoile2 g  l =
    List.fold_left (fun acc e -> if (mem_matrix e g)
				then acc = true
				else failwith "erreur position etoile ") true  l in

  let nom2 f =
    List.fold_left (fun acc (nm,seq) ->if  ( List.exists ( fun (n,x)-> n = nm)  n.fonctions )
				       then acc = true
				       else failwith "erreur nom fonction") true f
  in
  
  
  if case_valide n && (etoile2  n.grille n.etoiles ) && fonction2 n.fonctions && nom2 p  then
    ()
  else
   raise Tomber 
