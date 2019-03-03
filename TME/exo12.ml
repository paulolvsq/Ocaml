let plus_moins  =
  let m = Random.self_init(); Random.int 1000 in
  let rec loop m =
    print_string ("entrer une valeur\n");
    let a = read_int () in
    if compare a m < 0 then
      begin
	print_string("c'est plus\n");
	loop m
      end 
    else if compare a m = 0 then
      print_string("c'est gagné\n")
    else begin
	print_string("c'est moins\n");
	loop m 
      end 
  in loop m 

	  
      
  
	 
      
      
