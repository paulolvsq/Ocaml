type circle = {
  mutable x : int;
  mutable y : int;
  mutable radius : int
}

type temps = {
  mutable heures : int;
  mutable minutes : int;
  mutable secondes : int
}

let cpt = ref 0

let f () = (cpt := !cpt+1; g !cpt)

(*exercice 1*)

let translation c (dx, dy) =
  c.x <- c.x + dx;
  c.y <- c.y + dy

let symetrie_centrale c (a, b) = 
  translation c (2*(a-c.x), 2*(b-c.y))

let homothetie c (a, b) k = 
  translation c (2*k*(a-c.x), 2*k*(b-c.y));
  c.radius <- c.radius*k

(*exercice 2*)

type ('a, 'b) gtree = Node of 'a * ('b * ('a, 'b) gtree) list

let parcours_profondeur f t = 
  let pile = Queue.create () in
  let rec loop () =
    let e = Queue.pop pile in
    let Node (x, l) = e in
    (f x);
    List.iter (fun x -> Stack.push x pile) l;
    loop () with Queue.Empty -> () in 
    Queue.push t pile;
    loop() 

exception Found of int 

let mot_le_plus_court d = 
  parcours_largeur (fun (b, _) p -> if b then raise (Found p)) d
with Found x -> x

let pop_util p q = 
  let restant = Queue.create () in
  let rec loop () = 
    let e = Queue.pop q in
    if f e then (e, restant) else (Queue.push)
