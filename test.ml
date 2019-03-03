let factorielle n = 
  let rec loop n r = 
    if n = 0 then r
    else loop (n-1) (n*r) in loop n 1


let puissance x n = 
  let rec loop n acc = 
    if n = 0 then acc
    else loop (n-1) (acc*x) in loop n 1

xlet somme_des_chiffres n = 
  let rec loop n acc = 
    if (n/10) = 0 then acc+n
    else loop (n/10) (acc+(n mod 10)) in loop n 0

let divisible_par_3 n =  
  if somme_des_chiffres n mod 3 = 0 then true
  else false

let somme x y = 
  let rec loop x y = 
    if x = 0 then y 
    else if y = 0 then x
    else loop (x+1) (y-1) in loop x y

let produit x y = 
  let rec loop x acc = 
    if x = 0 then acc
    else loop (x-1) (acc+y) in loop x 0

let pgcd x y = 
  let rec loop x y = 
    if x mod y = 0 then y
    else loop y (x mod y) in loop x y

let fibonacci n = 
  let rec loop n acc1 acc2 = 
    if n = 0 then acc1
    else if n = 1 then acc2
    else loop (n-1) acc2 (acc2 + acc1) in loop n 0 1

let rec nb_chiffres n = 
  let rec loop n acc = 
    if (n / 10 = 0) then acc
    else loop (n / 10) (acc + 1) in loop n 1

let nb_diviseurs n = 
  let rec loop acc cpt = 
    if n = acc then cpt+1
    else if n mod acc = 0 then loop (acc+1) (cpt+1)
    else loop (acc+1) cpt in loop 1 0

let est_premier n = 
  if nb_diviseurs n = 2 then true
  else false

let facteurs_premiers n = 
  let rec loop n y liste = 
    if est_premier n then liste@[n]
    else if n mod y = 0 then loop (n / y) y liste@[y]
    else loop n (y+1) liste in loop n 2 []
 
let est_parfait n = 
  let rec loop n cpt acc = 
    if acc = n then true
    else if cpt = 0 then false
    else if n mod cpt = 0 then loop n (cpt-1) (acc+cpt)
    else loop n (cpt-1) acc in loop n (n-1) 0

let liste_diviseurs n = 
  let rec loop n liste acc = 
    if acc = n then liste@[n]
    else if n mod acc = 0 then loop n (liste@[acc]) (acc+1)
    else loop n liste (acc+1) in loop n [] 1

let liste_premiers_reverse n = 
  let rec loop n liste = 
    if n = 2 then liste@[2]
    else if nb_diviseurs n = 2 then loop (n-1) (liste@[n])
    else loop (n-1) liste in loop n []

let liste_premiers n = 
  List.rev (liste_premiers_reverse n)

let longueur_liste liste = 
  let rec loop liste acc = 
     if liste = [] then acc
     else loop (List.tl liste) (acc+1) in loop liste 0

let somme_liste liste = 
  let rec loop liste acc = 
    match liste with [] -> acc
      | _ -> loop (List.tl liste) (acc+List.hd liste) in loop liste 0

let miroir_liste liste = 
  let rec loop liste miroir = 
    match liste with [] -> miroir 
      | head::tail -> loop tail miroir@[head] in loop liste []
 
let max x y = 
  if x < y then y
  else x

let max_liste liste =
  let rec loop liste acc = 
    match liste with [] -> acc
      | head::tail -> loop tail (max acc head) in loop liste 0

let est_element liste x = 
  let rec loop liste acc = 
    match liste with [] -> false
      | head::tail -> if head = x then true else loop tail head in loop liste 0

let acces liste n = (*acc¨¨s ¨¤ la valeur donn¨¦e en param¨¨tre, pas ¨¤ l'indice*)
  let rec loop liste acc = 
    match liste with [] -> failwith "¨¦l¨¦ment n'est pas dans la liste"
      | head::tail -> if head = n then acc else loop tail (acc+1) in loop liste 0


let max_liste liste = 
  let rec loop liste acc = 
    match liste with 
      | [] -> acc 
      | head::tail -> if head >= acc then loop tail head 
	else loop tail acc in loop liste 0 

let moyenne_liste liste = 
  let rec loop liste acc nb = 
    match liste with 
      | [] -> acc/.nb
      | head::tail -> loop tail (acc+.head) (nb+.1.0) in loop liste 0.0 0.0

let pgcd x y = 
  let rec loop x y = 
    if (x mod y = 0) then y
    else loop y (x mod y) in loop x y

let note etud corr = 
  let rec loop etud corr acc = 
    match etud, corr with 
      | [], [] -> acc
      | h1::t1, h2::t2 -> if h1 = h2 then loop t1 t2 (acc+1)
	else loop t1 t2 acc in if List.length (etud) <> List.length (corr) then failwith "taille incorrecte"
	  else loop etud corr 0

let valide etud corr = 
  let res = note etud corr in 
  res >= ((List.length corr)/2)

type 'a btree = 
  | Empty 
  | Node of 'a * 'a btree * 'a btree

(*les insertions dans un arbre binaire de taille quelconque*)

let rec insert x b = 
  match b with 
    | Empty -> Node (x, Empty, Empty)
    | Node (y, g, d) -> if x > y then Node (y, g, insert x d)
      else Node (y, insert x g, d) 

(*convertir une liste en arbre binaire*)

let rec from_list l = 
  match l with 
    | [] -> Empty
    | head::tail -> insert head (from_list tail)

(*dire si une valeur est dans l'arbre*)

let rec mem b x = 
  match b with 
    | Empty -> false 
    | Node (y, g, d) -> if x = y then true
      else match (mem g x) with 
	| false -> mem d x
	| true -> true

(*convertir un arbre binaire en liste*)

let rec to_list b = 
  match b with 
    | Empty -> []
    | Node (x, g, d) -> to_list g @ [x] @ to_list d

(*faire le tri d'une liste en passant par un arbre : 
  on convertit vers une liste ¨¤ partir d'une liste pour obtenir une liste tri¨¦e*)

let tri l = 
  to_list (from_list l)

let affiche_tab_int tab =
  let rec loop tab =
    match tab with
    | [] -> failwith "fin du tableau"
    | h::t -> print_int h;
              print_newline();
              loop t
  in
  loop tab

let verif p n liste =
  let rec loop liste cpt acc =
    match liste with
    | [] -> acc
    | h::t -> if ((p h) && (cpt = n)) then h
              else if ((p h) && (cpt < n)) then loop t (cpt+1) h
              else loop t cpt acc
  in
  loop liste 0 0

let dernier_elem p liste =
  let init = List.find p liste in
  let rec loop liste acc =
    match liste with
    | [] -> acc
    | h::t -> if p h then loop t h
              else loop t acc
  in
  loop liste init

let reverse_list liste =
  let rec loop liste acc =
    match liste with
    | [] -> acc
    | h::t -> loop t (h::acc)
  in
  loop liste []
    
let reverse_list2 liste =
  List.fold_left (fun acc t -> (t::acc)) [] liste

let swap a b =
  let c = !a in
  a := !b;
  b := c
    
let affiche_nombres n =
  let x = ref n in
  while !x > 0
  do
    print_int !x;
    print_char ' ';
    x := (!x - 1);
  done
    
let max_array tab =
  let max = ref tab.(0) in
  let taille = Array.length tab in
  for i = 0 to (taille - 1)
  do
    if tab.(i) > !max then max := tab.(i)
  done;
  print_int !max
    
let max_array2 tab =
  Array.fold_left (fun acc e -> if e > acc then e else acc) tab.(0) tab

let afficher_liste liste =
  let rec loop liste =
    match liste with
    | [] -> []
    | h::t -> print_int h; loop t
  in
  loop liste
    
let to_list tab =
  let liste = ref [] in
  for i = 0 to ((Array.length tab) - 1)
  do
    liste := (!liste)@(tab.(i))
  done;
  afficher_liste !liste
    
let to_tab liste =
  let tab = Array.make (List.length liste) 0 in
  let rec loop liste cpt =
    match liste with
    | [] -> tab
    | h::t -> begin
        tab.(cpt) <- h;
        loop t (cpt + 1)
      end
  in
  loop liste 0
                                 
let cherche f tab =
  let i = ref 0 in
  let bool = false in
  while not bool
  do
    if (f tab.(i)) then trouve := true;
    i := !i + 1
  done;
  bool

let fibo_ite n =
  let u0 = ref 0 in
  let u1 = ref 1 in
  let u2 = ref 1 in
  if n = 0 then !u0
  else
    begin
      for i = 2 to n
      do
        u2 := !u0 + !u1;
        u0 := !u1;
        u1 := !u2
      done;
      !u2      
    end    
    
type 'a btree =
  | Nil
  | Node of 'a * 'a btree * 'a btree

let parcours t =
  try 
    let q = Queue.create () in
    let l = ref [] in
    Queue.push t q;
    let rec loop () =
      let top = Queue.pop q in
      match top with
      | Nil -> ()
      | Node (e, g, d) -> begin
          l := !l@[e];
          Queue.push g q;
          Queue.push d q;
          loop ()
        end
    in
    loop ()
  with
  | _ -> print_string "Arbre vide"

type lexeme =
  | Nombre of float
  | Op_binaire of (float -> float -> float)
  | Op_unaire of (float -> float)

let calcule liste =
  let p = Stack.create () in
  let rec loop liste =
    match liste with
    | [] -> ()
    | h::t -> begin
        match h with
        | Nombre(n) -> (Stack.push n p;
                       loop t)
        | Op_binaire(m) -> (let a = Stack.pop p in
                            let b = Stack.pop p in
                            Stack.push (m a b) p;
                            loop t)
        | Op_unaire(o) -> (let a = Stack.pop p in
                           Stack.push (o a) p;
                           loop t)
      end
  in
  loop liste;
  print_float (Stack.pop p)

    
