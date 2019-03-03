let rec bale (n:float) =
  if n = 1. then 1.
  else 1./.(n*.n)+.bale (n-.1.)

let p (n:float) =
  let rec loop n (acc:float) =
    if n = 1. then acc
    else loop (n -. 1.) (1./.(n*.n)+.acc)
  in
  loop n 1.
       
