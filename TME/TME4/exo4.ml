type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree

type 'a option = 
  | None 
  | Some of 'a

type value = Integer of int | Boolean of bool

					   
let rec insert x b = 
  match b with 
    | Empty -> Node(x, Empty, Empty)
    | Node(y, g, d) -> if x > y then Node(y, g, insert x d)
		       else Node(y, insert x g, d)
				

let rec from_list (l: 'a list) =
  match l with
  | [] -> Empty
  | h::t -> insert h (from_list t)


		   
let rec mem b x =
  match b with
  | Empty -> false
  | Node(y,g,d)-> if x= y then true
		  else match (mem g x) with
		       | false -> mem d x
		       | true -> true 


let rec to_list b =
  match b with
  | Empty -> []
  | Node(x,g,d)-> to_list g @ [x] @ to_list d


let rec tri l =
  to_list (from_list l)
  
