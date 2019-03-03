let cat_file f = 
  let ic = open_in f in
  let rec loop () = 
    try 
      print_char(input_line ic)::(loop ())
    with End_of_file -> (close ic; [])
  in loop () 

let cat_file f = 
  let ic = open_in f in
  let rec loop ls = 
    let r = (try Some(input_line ic)
      with End_of_file -> None) in 
    match r with 
      None -> close_in ic
	| Some c -> (loop (r::ls))
  in List.rev (loop []) 

(*fonctions pour les piles fifo et lifo : create, add, pop*)

let bfsearch (p : 'a -> bool) (bts : 'a tree) : 'a =
  let rec loop (bts : ('a btree) Queue.t) : 'a = 
    if (Queue.is_empty bts) then raise Not_found
    else match (Queue.pop bts) with
	Empty -> (loop bts)
      | Node (x, bt1, bt2) -> (if (p x) then x
	else (Queue.add bt1 bts;
	      Queue.add bt2 bts;
	      loop bts)) 
  in let bts = Queue.create () in
     Queue.add bt bts;
     loop bts

type 'a gtree =
    GEmpty 
  | GNode of 'a * ('a gtree) list

let rec gtree_mem x gt = 
  match gt with 
    | GEmpty -> false 
    | GNode (z, gts) -> if (z=x) then true
      else (gtree_mems x gts) and gtree_mem x gts = 
  match gts with 
    | [] -> false 
    | gt::gts -> (gtree_mem x gt) || (gtree_mems x gts)

(*listes chainées*)

type 'a cell = { 
  content : 'a;
  next : 'a cell
}

type point = {
  abs : int;
  ord : int;
}

let p = { abs = 0; ord = 0} in 
Printf.printf "(%d, %d)" x.abs x.ord 

let move p dx dy = 
  p.abs <- p.abs + dx;
  p.ord <- p.ord + dy;
  p


