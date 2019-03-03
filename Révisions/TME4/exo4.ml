type 'a btree = 
  | Empty 
  | Node of 'a * 'a btree * 'a btree

let rec insert b x = 
  match b with 
    | Empty -> Node (x, Empty, Empty) 
    | Node (y, g, d) -> if x < y then Node (y, insert g x, d)
      else Node (y, g, insert d x)

let rec taille b = 
  match b with 
    | Empty -> 0 
    | Node (x, g, d) -> 1 + taille g + taille d

let rec hauteur b = 
  match b with 
    | Empty -> 0
    | Node (x, g, d) -> 1 + (max (hauteur g) (hauteur d))
 
let rec map f b = 
  match b with 
    | Empty -> Empty 
    | Node (x, g, d) -> Node (f x, map f g, map f d)

type 'a option = 
  | None 
  | Some of 'a 

let rec assoc x b = 
  match b with  
    | Empty -> None
    | Node ((k, v), g, d) -> if k = x then Some v
      else match (assoc x g) with 
	| None -> assoc x d
	| Some y -> Some y

let rec vers_liste b = 
  match b with 
    | Empty -> [] 
    | Node (x, g, d) -> vers_liste g @ [x] @ vers_liste d

type card = (valeur * couleur) and 
  couleur = Pique | Trefle | Carreau | Coeur and 
  valeur = As | Roi | Dame | Valet | Num of int 

let points_of_card (v, c) = 
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

let points_of_hand liste =  
  List.fold_left (fun acc x -> (points_of_card (x + acc))) 0 liste

let rec from_list liste = 
  match liste with 
    | [] -> Empty 
    | head::tail -> insert (from_list tail) head

let rec mem b x = 
  match b with 
    | Empty -> false 
    | Node (y, g, d) -> if x = y then true 
      else match (mem g x) with 
	| false -> mem d x
	| true -> true

let rec to_list b = 
  match b with 
    | Empty -> [] 
    | Node (x, g, d) -> to_list g @ [x] @ to_list d 

let tri liste = 
  to_list (from_list liste)
