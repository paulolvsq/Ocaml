type bureau = {
  inscrits : int;
  votes : (string * int) list
}

let somme_votes votes = 
  let rec loop votes acc = 
    match votes with 
      | [] -> acc
      | (nom, nb_voix)::t -> loop t (acc + nb_voix)
  in
  loop votes 0

let somme_inscrits bl = 
  let rec loop bl acc = 
    match bl with 
      | [] -> acc
      | (inscrits, votes)::t -> loop t (acc + inscrits)
  in
  loop bl 0

let add_voix_candidat nom voix votes = 
  let rec loop votes acc = 
    match votes with 
      | [] -> acc 
      | (n, v)::t -> if (int_of_string n) = (int_of_string nom) 
	then acc@(n, v + voix)::t
	else loop t acc@[(n, v)]
  in
  loop votes []

let add_voix_bureau votes_bureau votes_circo = 
  let rec loop votes_bureau acc = 
    match votes_bureau with 
      | [] -> acc 
      | (nom, nb_voix)::t -> loop t (acc@(add_voix_candidat nom nb_voix votes_circo))
  in
  loop votes_bureau []

let votes_circo bl = 
  let rec loop bl acc = 
    match bl with 
      | [] -> acc
      | (inscrits, votes)::t -> loop t (add_voix_bureau votes acc)
  in
  loop bl []

let premier_tour nb_voix nb_votes = 
  (float_of_int nb_voix) > (float_of_int nb_votes)/.2.

let elu_premier_tour votes nb_votes = 
  let rec loop votes =
    match votes with 
      | [] -> None 
      | (nom, nb_voix)::t -> if premier_tour nb_voix nb_votes then Some nom
	else loop t
  in
  loop votes

let second_tour nb_voix nb_inscrits = 
  (float_of_int nb_voix) > (float_of_int nb_inscrits)/.8.

let candidats_second_tour votes nb_inscrits = 
  let rec loop votes acc = 
    match votes with 
      | [] -> acc 
      | (nom, nb_voix)::t -> if second_tour nb_voix nb_inscrits 
	then loop t acc@[(nom, nb_voix)]
	else loop t acc
  in
  loop votes [] 

type resultat = 
    Elu_premier_tour of string 
  | Candidats_second_tour of string list 
