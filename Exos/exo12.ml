let plus_moins = 
  let mystere = Random.self_init (); 
          Random.int 1000 in
    let rec loop m = 
      print_string ("Entrez une valeur : \n");
      let a = read_int () in
      if compare a m < 0 then 
	begin 
	  print_string ("C'est plus \n");
	  loop m 
	end 
      else if compare a m > 0 then 
	begin 
	  print_string ("C'est moins \n");
	  loop m
	end 
      else if compare a m = 0 then print_string ("C'est gagné !\n")
    in loop mystere
