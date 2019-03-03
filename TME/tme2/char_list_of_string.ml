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


let char_list_of_string str =
  let rec aux i acc =
    match i with
    | -1 -> acc
    | _ -> aux (i-1) (str.[i] :: acc)
  in aux (String.length str - 1) []

let compare_char a b =
  let x = int_of_char a in
  let y = int_of_char b in
    
  if x = y then 0
  else if x < y then -1
  else 1

	 
let rec list_compare f l1 l2 =
  match l1,l2 with
  |[],[] -> 0
  |h::t, [] -> 1
  |[] ,h::t -> (-1)
  |h::t,x::xs -> if f h x = 0 then list_compare f t xs
		 else
		   f h x 

			
let ispalindrome l =
  list_compare compare_char l (List.rev l) = 0 
		 

let isanagram s1 s2 =
  let l1 = merge_sort (char_list_of_string s1) in
  let l2 = merge_sort (char_list_of_string s2) in
  list_compare compare_char l1 l2 = 0
  
