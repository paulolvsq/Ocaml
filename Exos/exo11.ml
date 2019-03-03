open Graphics;;

let milieu (x1, y1) (x2, y2) = 
  ((x1+x2)/2, (y1+y2)/2)

let dessine_segment (x,y) (z,k) =
  Graphics.moveto x y;
  Graphics.lineto z k

let dessine_triangle a b c = 
  dessine_segment a b;
  dessine_segment a c;
  dessine_segment b c

let rec dessine_sierpinski a b c n = 
  if n = 0 then dessine_triangle a b c
  else begin 
    let aprime = milieu b c in
    let bprime = milieu a c in 
    let cprime = milieu a b in
    dessine_sierpinski a bprime cprime (n-1);
    dessine_sierpinski aprime b cprime (n-1);
    dessine_sierpinski aprime bprime c (n-1);
  end 

let () =
(* initialisation de la fenetre graphique *)
  Graphics.open_graph " 800x800";
  Graphics.set_window_title "Sierpinski";
  Graphics.loop_at_exit [] (fun _ -> ());
(* on construit le triangle initial *)
  let a = (50,50) in
  let b = (400,750) in
  let c = (750, 50) in
(* on le dessine *)
  let nb_iterations = 10 in
  dessine_sierpinski a b c nb_iterations
