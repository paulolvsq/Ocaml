type table = {
    names : string list;
    mutable records : (string array) list
  }

let index z xs =
  let rec loop liste acc =
    match liste with
    | [] -> raise Not_found
    | h::t -> if h = z then acc
              else loop t (acc+1)
  in
  loop xs 0

let indexes zs xs =
  let rec loop zs acc =
    match zs with
    | [] -> acc
    | h::t -> loop t ((index h xs)::acc)
  in
  loop zs []

let proj is r =
  let rec loop is acc =
    match is with
    | [] -> acc
    | h::t -> loop t ((r.(h))::acc)
  in
  List.rev (loop is [])

let select tbl ns =
  let rec loop enrg acc =
    match enrg with
    | [] -> acc
    | h::t -> loop t ((proj (indexes ns tbl.names) h)::acc)
  in
  loop tbl.records []
    
let satisfy tbl c r =
  let (n,p) = c in
  let indice = index n tbl.names in
  p r.(indice)

let find_one tbl c =
  let rec loop enrg =
    match enrg with
    | [] -> raise Not_found
    | h::t -> if satisfy tbl c h then h
              else loop t
  in
  loop tbl.records

let update tbl c n v =
  let indice = index n tbl.names in
  let ligne = find_one tbl c in
  ligne.(indice) <- v

let find_all tbl c =
  let rec loop enrg acc =
    match enrg with
    | [] -> acc
    | h::t -> if satisfy tbl c h then loop t (h::acc)
              else loop t acc
  in
  loop tbl.records []

let find_all2 (tbl:table) (cs: (string * (string -> bool)) list) : (string array) =
  assert false

    
let remove tbl c =
  let retirer = find_all tbl c in
  
