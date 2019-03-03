type cell = NA
          | Num of float
          | CRef of (int*int)
          | App of (float -> float -> float) * cell * cell
          | Sum of (int*int) * (int*int)

type fcalc = (cell array) array

let expand_pos (lig1, col1) (lig2, col2) =
  let acc = [] in
  if (lig2 < lig1) || (col2 < col1) then acc
  else
    let rec lig i acc =
      let rec col i j acc2 =
        if j = col2 then (i,j)::acc2
        else col i (j+1) ((i,j)::acc2)
      in
      if i = lig2 then col i col1 acc
      else lig (i+1) (col i col1 acc)
    in
    List.rev (lig lig1 []) 

let expand f (lig1, col1) (lig2, col2) =
  let liste = expand_pos (lig1, col1) (lig2, col2) in
  (*List.fold_left (fun acc (i,j) -> (f.(i).(j)::acc)) [] liste*)
  let rec loop liste acc =
    match liste with
    | [] -> acc 
    | (i,j)::t -> loop t (f.(i).(j)::acc)
  in
  loop liste [] 

    
let array_for_all p xs =
  Array.for_all p xs

let matrix_for_all p xss =
  Array.for_all (fun elm -> array_for_all p elm) xss

let check_cell (f:fcalc) (c:cell) : bool =
  assert false

let check_fclalc f =
  matrix_for_all check_cell f
  
let eval_cell f c =
  let rec loop c =
    match c with
    | NA -> 0.0
    | Num(f) -> f
    | CRef((i,j)) -> loop f.(i).(j)
    | App(f, c1, c2) -> f (loop c1) (loop c2)
    | Sum((i,j),(i2,j2)) -> begin
        
        let list = expand f (i,j) (i2, j2) in
        List.fold_left (fun acc elmCell -> acc +. (loop elmCell)) 0.0 list end 
  in
  loop c
    
