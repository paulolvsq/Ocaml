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
  match l1, l2 with 
    | [], [] -> 0
    | h::t, [] -> 1
    | [], h::t -> -1 
    | h1::t1, h2::t2 -> if f h1 h2 = 0 then list_compare f t1 t2
      else f h1 h2

let is_palindrome l = 
  list_compare compare_char l (List.rev l) = 0
