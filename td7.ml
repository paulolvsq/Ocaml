(*exercice 1*)

let inverse tab x y = 
  let tmp = tab.(x) in
  tab.(x) <- tab.(y);
  tab.(y) = tmp

let memory tab v = 
  let acc = ref false in
  for i = 0 to Array.length (tab) - 1 do
    acc := (v = tab.(i)) || (!acc)
  done;
  !acc
 
(*exercice 4*)

let print_matrix tab = 
  let h = Array.length (tab) - 1 in
  let l = Array.length (tab) - 1 in
  for i = 0 to l do
    for j = 0 to h do
      print_int tab.(i).(j);
      print_string ("\t")
    done;
    print_newline ();
  done

let generate_matrix n = 
  let matrix = Array.make_matrix n n 0 in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      matrix.(i).(j) <- Random.int 100
    done;
  done;
  matrix
