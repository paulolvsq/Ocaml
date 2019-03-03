let merge l1 l2 =
  let rec loop l1 l2 acc =
    match l1, l2 with
    | [], [] -> List.rev acc
    |h::t, [] -> (List.rev acc)@l1
    |[],h::t -> (List.rev acc)@l2
    | h::t,x::xs -> if h> x then loop l1 xs (x::acc)
		    else loop t l2 (h::acc)
  in
  loop l1 l2 []
 
let rec split l =
  match l with
  | [] -> [],[]
  | x::[] -> [x],[]
  | h::x::t -> let g,d = split t in
	       (h::g),(x::d)

let rec merge_sort l =
  match l with
  | [] -> []
  | [x] -> l
  | head::tail -> let g,d = split l in
		  let g = merge_sort g in
		  let d = merge_sort d in
		  merge g d
