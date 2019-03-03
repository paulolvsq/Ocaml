(** Redéfinition du module Queue, permettant un accès à la fin de la
   file d'attente (la tête du serpent) *)
module Queue =
struct
  exception Empty

  type 'a c = { content: 'a; mutable next: 'a cell }
  and 'a cell =
    | Nil
    | Cons of 'a c

  type 'a t = {
    mutable length: int;
    mutable first: 'a cell;
    mutable last: 'a cell
  }

  (** Return a new queue, initially empty. *)
  let create () = {
    length = 0;
    first = Nil;
    last = Nil
  }

  (** Discard all elements from a queue. *)
  let clear q =
    q.length <- 0;
    q.first <- Nil;
    q.last <- Nil

  (** [push x q] adds the element [x] at the end of the queue [q]. *)
  let push x q =
    let cell = Cons {
        content = x;
        next = Nil
      } in
    match q.last with
    | Nil ->
      q.length <- 1;
      q.first <- cell;
      q.last <- cell
    | Cons last ->
      q.length <- q.length + 1;
      last.next <- cell;
      q.last <- cell

  (** [top q] returns the first element in queue [q], without removing
   it from the queue, or raises {!Empty} if the queue is empty. *)
  let top q =
    match q.first with
    | Nil -> raise Empty
    | Cons { content } -> content

  (** [bottom q] returns the last element in queue [q], without removing
   it from the queue, or raises {!Empty} if the queue is empty. *)
  let bottom q =
    match q.last with
    | Nil -> raise Empty
    | Cons { content } -> content

  (** [pop q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. *)
  let pop q =
    match q.first with
    | Nil -> raise Empty
    | Cons { content; next = Nil } ->
      clear q;
      content
    | Cons { content; next } ->
      q.length <- q.length - 1;
      q.first <- next;
      content

  (** Return [true] if the given queue is empty, [false] otherwise. *)
  let is_empty q = q.length = 0

  (** Return the number of elements in a queue. *)
  let length x = x.length

  (** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The queue itself is unchanged. *)
  let iter =
    let rec iter f cell =
      match cell with
      | Nil -> ()
      | Cons { content; next } ->
        f content;
        iter f next
    in
    fun f q -> iter f q.first

  (** [fold f accu q] is equivalent to [List.fold_left f accu l],
   where [l] is the list of [q]'s elements. The queue remains
   unchanged. *)
  let fold =
    let rec fold f accu cell =
      match cell with
      | Nil -> accu
      | Cons { content; next } ->
         let accu = f accu content in
         fold f accu next
    in
    fun f accu q -> fold f accu q.first

   (** [exists f q] is equivalent to [List.exists f l],
   where [l] is the list of [q]'s elements. The queue remains
   unchanged. *)
  let exists =
    let rec exists f cell =
      match cell with
      | Nil -> false
      | Cons { content; next } ->
         f content ||
         exists f next
    in
    fun f q -> exists f q.first

end

(******************************************************************************)
                             (******************)
                             (* THE SNAKE GAME *)
                             (******************)
(******************************************************************************)

(*********)
(* types *)
(*********)

(* le type des points en 2 dimensions *)
type point = {x : int; y : int}

(* le types des 4 directions possibles: nord, sud, est, ouest*)
type dir   =  N | S | E | W

(* Un "snake" peut-etre vu comme une file d'attente de point *)
type snake = point Queue.t

(* Un monde est composé d'un snake, une direction ver laquelle il se dirige
   et un point représentant la prochaine nourriture à manger *)
type world = {
    s : snake;
    mutable pomme : point;
    mutable orientation : dir
  }

(***************)
(* utilitaires *)
(***************)

(* tirage d'un point aléatoire *)
let rand_point () = {
    x = Random.int 20;
    y = Random.int 20
  }

(* assure que le snake ne sort pas du cadre avec un déplacement torique :
   quand il "sort" de l'écran par la droite, il revient par la gauche *)
let tore (x : int) : int = (x + 20) mod 20

(********************)
(* Code à compléter *)
(********************)

(* rotation d'une direction vers la gauche *)
let gauche (d : dir) : dir  =
  (* remplacer le assert false par votre code *)
  match d with
  | N -> W
  | E -> N
  | S -> E
  | W -> S
	  

(* rotation d'une direction vers la droite *)
let droite (d : dir) : dir =
  (* remplacer le assert false par votre code *)
  match d with
  | N -> E
  | E -> S
  | S -> W
  | W -> N
	  

(* déplacement d'un point vers une direction. On respectera le déplacement
   torique, en utilisant la fonction "tore" sur les abscisses et ordonnées
   obtenues *)
let deplace_point (p : point) (d : dir) : point =
  (* remplacer le assert false par votre code *)
  match d with
  |  N -> {x = p.x; y = tore(p.y+1)}
  |  S -> {x = p.x ;y = tore (p.y-1)}
  |  E -> {x = tore (p.x+1);y = p.y}
  |  W -> {x = tore (p.x-1);y = p.y}
    

(* renvoie vrai si la tete du serpent et la pomme sont à la même position *)
let peut_manger (s : snake) (pomme : point) : bool =
  (* remplacer le assert false par votre code *)
  let tete = Queue.bottom s in
  match tete,pomme with
  | xy, ab -> xy = ab
  
(* deplacement du serpent *)
let deplace_snake (s : snake) (d : dir) (mange : bool) : unit =
  (* remplacer le assert false par votre code *)
  let tete = Queue.bottom s in
  let nvlletete = deplace_point tete d in
  Queue.push nvlletete s;
  if not mange
  then
    let _ = Queue.pop s in
    ()
  else ()

(* prend un serpent et retourne vrai si sa tete est à la meme position qu'une
   des cases de son corps *)
let perdu (m : snake) : bool =
  (* remplacer le assert false par votre code *)
  let tete = Queue.bottom m in
  let acc = ref 0 in
  let _ = (Queue.iter (fun p1 -> if p1 = tete then acc:=!acc+1) m) in
  !acc > 1

let une_etape (w : world) : unit =
  (* remplacer le assert false par votre code *)
  if peut_manger w.s w.pomme
  then
    begin
      deplace_snake w.s w.orientation true;
      w.pomme<-rand_point ()
    end
  else deplace_snake w.s w.orientation false ;	     
  if perdu w.s
  then raise (Exit)
  else ()
  

(**********************************)
(* Gestion des evenements clavier *)
(**********************************)

let kb_input (w : world) =
  if Graphics.key_pressed () then
    let c = Graphics.read_key () in
    match c with
    | 'd' -> w.orientation <- droite w.orientation
    | 'q' -> w.orientation <- gauche w.orientation
    | _   -> ()

(**********)
(* Dessin *)
(**********)

let draw (w : world) =
  Graphics.clear_graph ();
  let sx = 40 and sy = 40 in
  let draw (p : point) =
    Graphics.set_color Graphics.blue;
    Graphics.fill_rect (p.x * sx) (p.y * sy) sx sy;
  in
  let draw_head (p : point) =
    Graphics.set_color Graphics.green;
    Graphics.fill_circle (p.x * sx + sx / 2) (p.y * sy + sy / 2) (sx /2)
  in
  let draw_food (p : point) =
    Graphics.set_color Graphics.red;
    Graphics.fill_circle (p.x * sx + sx / 2) (p.y * sy + sy / 2) (sx /2)
  in
  Queue.iter draw w.s;
  draw_head (Queue.bottom w.s);
  draw_food (w.pomme)

(* fonctions d'attente *)
let sleep millis = ignore(Sys.command ("sleep "^(string_of_float millis)))

(* plus le serpent est grand, moins on attend*)
let sleep_time n =  (0.2 /. (1.05 ** float_of_int n))

(* boucle d'interaction pricipale *)
let rec loop (w : world) =
  (* on change la direction par rapport à la touche appuyée*)
  kb_input w;
  (* on fait avancer le monde d'une étape*)
  une_etape w;
  (* on attend un peu *)
  sleep (sleep_time (Queue.length w.s));
  (* on affiche à l'écran *)
  draw w;
  (* et on recommence *)
  Graphics.synchronize();
  loop w

let _ =
  (* initialisation d'un serpent de taille 2, qui va vers le haut,
     avec une pomme placée aléatoirement *)
  let a = {x = 1; y = 5}
  and b = {x = 0; y = 5} in
  let q = Queue.create () in
  Queue.push a q;
  Queue.push b q;
  let dir_init = N in
  let monde_init = {s = q ; pomme = rand_point() ; orientation = dir_init} in

  (* création de la fenetre *)
  Graphics.open_graph " 800x800";
  Graphics.auto_synchronize false;
  (* on entre dans la boucle d'interraction *)
  loop monde_init
