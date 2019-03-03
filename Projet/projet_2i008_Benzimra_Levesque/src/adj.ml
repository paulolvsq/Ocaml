(* les listes d'adjascence : Elles sont triées *)
type 'a t = (int * 'a) list

let empty = []

let rec mem (cle:int) (liste : 'a t) : bool =
  match liste with
  |[]->false
  |(i,h)::t->if i = cle then true
             else if i > cle then false
             else mem cle t 
               
let rec get (cle:int) (liste:'a t) : 'a =
      match liste with
      | []->raise(Not_found)
      | (i,v)::t-> if i = cle then v
                   else if i>cle then raise(Not_found)
                   else get cle t

let set (cle:int) (valeur:'a) (liste:'a t) : 'a t =
  let rec loop l =
    match l with
    | []-> [cle,valeur]
    | (a,h)::t->if a < cle then
              ((a,h)::loop t)
                else if a = cle then
                  (a,valeur)::t
                else
               (cle,valeur)::l
  in loop liste 
   
           
(*************************)
(* matrices d'adjascence *)
(*************************)

type 'a matrix = ('a t) t

let set_matrix ((i,j):int*int) (elm:'a) (matrice: 'a matrix) : 'a matrix =
  try
    let ligne  = get i matrice in
    let lignecomplete = set j elm ligne in
    set i lignecomplete matrice
  with
  |Not_found -> set i [(j,elm)] matrice
    
let get_matrix ((i,j):int*int) (m:'a matrix) : 'a =
  let ligne  = get i m in
  get j ligne 


let mem_matrix ((i,j):int*int) (m:'a matrix) : bool =
try
  let ligne  = get i m in
  mem j ligne 
with
|Not_found->false

let bornes (m:'a matrix) : (int * int) * (int * int) =
  if m = [] then
    failwith "noob"
  else    
    let min_max_x m = List.fold_left ( fun (a,b) (x,elem) -> (min a x , max b x) ) (max_int, min_int) m in
    let min_max_y m = List.fold_left ( fun (a,b) (_,(ligne)) -> ( min a (let (c,d)=  min_max_x ligne in c), max b (let (f,g) = min_max_x ligne in g) )) (max_int,min_int) m in 
    (min_max_x m),(min_max_y m)
