let op_list = ["+";"-";"/";"*";"affiche";"sortir"] 

let is_operator c =
  List.mem c op_list

let eval_op l c =
  match l,c with
  |l,"sortir"-> raise Exit 
  |h::x::t,"affiche" ->List.iter print_int l ;
		       l
  |h::x::t,"+" -> (h+x)::t
  |h::x::t,"-" ->(h-x)::t
  |h::x::t,"/" ->(h/x)::t
  |h::x::t,"*" ->(h*x)::t	
  |_,_ -> raise (Invalid_argument "c")		  

let rec loop l =
  print_string ("entrer une quelque chose\n");
  let c = read_line() in
  if is_operator c then
    loop (eval_op l c)
  else
    loop (int_of_string(c)::l)
		 
  
let main =
  loop []
