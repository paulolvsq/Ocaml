type cell = NA
  | Num of float
  | CRef of (int*int)
  | App of (float -> float -> float) * cell * cell
  | Sum of (int*int) * (int*int)

type fcalc = (cell array) array
               
let expand_pos (lig1, col1) (lig2, col2) =
  if (lig2 < lig1) || (col2 < col1) then []
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
  let rec loop liste acc =
    match liste with
    | [] -> acc
    | (i,j)::t -> loop t ((i,j)::acc)
  in
  loop liste []

let check_cell (f:fcalc) (c:cell) : bool =
  assert false 
                                     
let array_for_all p xs =
  Array.for_all p xs
              
let matrix_for_all p xss =
  Array.for_all (fun elm -> array_for_all p elm) xss

let check_fcalc f =
  matrix_for_all (fun elm -> check_cell f elm) f
