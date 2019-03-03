type bureau = {
    inscrits : int;
    votes : (string*int) list
  }
                
let somme_votes votes =
  let rec loop votes acc =
    match votes with
    | [] -> acc
    | (nom,voix)::t -> loop t (acc+voix)
  in
  loop votes 0

let somme_inscrits bl =
  let rec loop bl acc =
    match bl with
    | [] -> acc
    | (i,v)::t -> loop bl (acc+i)
  in
  loop bl 0

let add_voix_candidat nom voix votes =
  let rec loop votes acc =
    match votes with 
    | [] -> (nom,voix)::acc
    | (n,v)::t -> if (int_of_string nom) = (int_of_string n) then acc@[(n,v)]@t
                  else loop t ((n,v)::acc)
  in
  loop votes []

let add_voix_bureau votes_bureau votes_circo =
  let rec loop votes_bureau acc =
    match votes_bureau with
    | [] -> acc
    | (n,v)::t -> loop t ((add_voix_candidat n v votes_circo)::acc)
  in
  loop votes_bureau []

let votes_circo bl = 
  let rec loop bl acc = 
    match bl with 
    | [] -> acc
    | (i,v)::t -> loop t (add_voix_bureau v acc)
  in
  loop bl []

let premier_tour nb_voix nb_votes =
  (float_of_int nb_voix) > ((float_of_int nb_votes)/.2.)

let elu_premier_tour votes nb_votes =
  let rec loop votes acc =
    match votes with
    | [] -> acc
    | (nom,nb_voix)::t -> if premier_tour nb_voix nb_votes
                          then loop t ((nom,nb_voix)::acc)
                          else loop t acc
  in
  loop votes []
    
let second_tour nb_voix nb_inscrits =
  (float_of_int nb_voix) > ((float_of_int nb_inscrits)/.8.)

let candidats_second_tour votes nb_inscrits =
  let rec loop votes acc =
    match votes with
    | [] -> acc
    | (nom,nb_voix)::t -> if second_tour nb_voix nb_inscrits
                          then loop t ((nom,nb_voix)::acc)
                          else loop t acc
  in
  loop votes []
    
