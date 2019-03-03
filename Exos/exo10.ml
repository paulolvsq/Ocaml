let rec bale (n : float) = 
  if n = 1. then 1.
  else (1./.(n*.n)) +. bale (n-.1.)

let bale2 (n : float) = 
  let rec loop n (acc : float) =
    if n = 1. then acc
    else loop (n-.1.) (acc+.(1./.(n*.n))) in loop n 1.

