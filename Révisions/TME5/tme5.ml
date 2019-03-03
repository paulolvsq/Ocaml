(*exercice 1*)

type ('a, 'b) gtree = Node of 'a * ('b * ('a, 'b) gtree) list 

type dict = (bool, char) gtree

let empty_dict = Node (false, [])

let is_leaf arbre = 
  match arbre with 
    | Node (false, _) -> true 
    | _ -> false

let rec hauteur arbre = 
  match arbre with 
    | Node (_, []) -> 0 
    | Node (_, l) -> 1 + hauteur_liste l 
and hauteur_liste l = 
  match l with 
    | [] -> 0 
    | (c, a)::tail -> max (hauteur a) (hauteur_liste tail) 

let rec taille arbre = 
  match arbre with 
    | Node (_, []) -> 1
    | Node (_, l) -> 1 + taille_liste l
and taille_liste liste = 
  match liste with 
    | [] -> 0
    | (c, a)::tail -> taille a + taille_liste tail

let rec assoc_ord c cl = 
  match cl with 
    | [] -> None 
    | (k, v)::tail -> if k = c then Some v
      else if c > k then None
      else assoc_ord c tail

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

let rec occurrence char freqs = 
  match char, freqs with 
    | _, [] -> ([char, 1])
    | x, (k, v)::tail -> if x = k then (k, v+1)::tail
      else (k, v)::occurrence char tail

let est_minuscule lettre = 
  int_of_char lettre <= 122 && int_of_char lettre >= 97

