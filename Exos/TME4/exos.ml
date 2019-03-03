type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let rec taille_arbre arbre = 
  match arbre with
    | Empty -> 0
    | Node (x, g, d) -> 1 + taille g + taille d

let rec hauteur_arbre arbre = 
  match arbre with 
    | Empty -> 0
    | Node (x, g, d) -> 1 + (max (hauteur_arbre g) (hauteur_arbre d))

let rec map f b = 
  match b with 
    | Empty -> Empty 
    | Node (x, g, d) -> Node (f x, map f g, map f d) 

type 'a option = 
  | None 
  | Some of 

let (x : int option) = Some 3

let head l = 
  match l with 
    | [] -> None 
    | h::t -> Some h

let rec assoc x b = 
  match b with 
    | Empty -> None
    | Node ((k,v), g, d) -> if k = x then Some v 
      else match (assoc x g) with 
	| None -> assoc x d
	| Some y -> Some y

let rec insert x b = 
  match b with 
    | Empty -> Node(x, Empty, Empty)
    | Node(y, g, d) -> if x > y then Node(y, g, insert x d)
      else Node(y, insert x g, d)

type value = Integer of int | Boolean of bool

type expr = Add of expr * expr 
	    | Or of expr * expr 
	    | Val of value 
	    | If of expr * expr * expr 

let rec eval e = 
  match e with 
    | value v -> v
    | Add (expr1, expr2) -> begin match (eval expr1, eval expr2) with
	| Integer i1, Integer i2 -> Integer (i1+i2)
	| _ -> raise (Invalid_argument ("eval")) end
    | If (expr1, expr2, expr3) -> begin match (eval expr1) with
	| Boolean true -> eval expr2
	| Boolean false -> eval expr3
	| _ -> raise (Invalid_argument ("eval")) end
 

type card = (valeur*couleur) and 
  valeur = As | Roi | Dame | Valet | Num of int and 
couleur = Pique | Coeur | Trefle | Carreau 

let points_of_card (v,c) = 
  let m = match c with 
    | Pique | Trefle -> 1
    | _ -> -1 
  in 
  let pt = match v with 
    | As -> 150
    | Roi -> 100
    | Dame -> 80
    | Valet -> 50
    | Num i -> i
  in 

type hand = card list

let points_of_hand h = 
  List.fold_left (fun acc x -> (points_of_card (x + acc)) 0 h
