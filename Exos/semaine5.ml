type ('a, 'b) gtree = Node of 'a * ('b * ('a, 'b) gtree) list      

type dict = (bool, char) gtree

let empty_dict = Node (false, [])

let is_leaf a = 
  match a with 
    | Node (_, []) -> true
    | _ -> false

let rec pair x = 
  if x = 0 then true 
  else impair (x-1) 
and impair x = 
  if x = 0 then false 
  else pair (x-1)

let rec hauteur t = 
  match t with 
    | Node (_, []) -> 0
    | Node (_, l) -> 1 + hauteur_liste l
and hauteur_liste l = 
  match l with 
    | [] -> 0
    | (c, a)::xs -> max (hauteur a) (hauteur_liste xs) 

let rec taille t = 
  match t with 
    | Node (_, []) -> 1
    | Node (_, l) -> taille_liste l
and taille_liste l =  
  match l with 
    | [] -> 0 
    | (c, a)::xs -> taille a + taille_liste xs

let taille_courte t = 
  let Node (_, l) = t in 
  1 + List.fold_left (fun acc x -> taille x + acc) 0 t

let rec assoc_ord c cl =
  match cl with 
    | [] -> None 
    | (k, v)::t -> if k = c then Some v
      else if c > k then None 
      else assoc_ord c t 
  
let rec find_subtree c d = 
  let Node (_, l) = d in assoc_ord c l

let rec mem d w = 
  match d, w with 
    | Node (c, []), [] -> c 
    | _, x::xw -> (match find_subtree x d with 
	| None -> false 
	| Some c -> mem c xw)

let rec add_word d w = 
  match (w, d) with 
    | [], Node (b, l) -> Node (true, l)
    | h::t, Node (b, l) -> Node (b, add_word_to_dict_list l w) 
and add_word_to_dict_list l w = 
  match (w, l) with 
    | [], _ -> l
    | h::t, [] -> [(h, add_word empty_dict t)]
    | h::t, (c, d)::xd -> if h = c then (c, add_word d t)::xd
      else if c > h then (h, add_word empty dict d)::l
      else  (c, d)::add_word_to_dict_list xd w
