open Graphics


let milieu (x,y) (a,b) =
  ( ((x+a)/2),((y+b)/2))
    

let dessine_segment (x,y) (z,k) =
Graphics.moveto x y;
Graphics.lineto z k

	
let dessine_triangle a b c =
  dessine_segment a b;
  dessine_segment a c;
  dessine_segment b c
		  
let rec dessine_sierpinski a b c n =
  if n = 0 then
    dessine_triangle a b c
  else begin
      let z = milieu b c in
      let r = milieu a c in
      let p = milieu a b in
      dessine_sierpinski  a r p (n-1);
      dessine_sierpinski z b p (n-1);
      dessine_sierpinski z r c (n-1);
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
	
