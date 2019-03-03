type table = {
    names : string list;
    mutable records : (string array) list
}

let index z xs =
  let rec loop xs acc =
    match xs with
    | [] -> raise Not_found
    | h::t -> if z = h then acc
              else loop t (acc + 1)
  in
  loop xs 0

let indexes zs xs =
  let rec loop zs acc =
    match zs with
    | [] -> acc
    | h::t -> loop t acc@[(index h xs)]
  in
  loop zs []

let proj liste_entiers str_tab =
  let rec loop liste_entiers acc =
    match liste_entiers with
    | [] -> acc
    | h::t -> loop t acc@[(str_tab.(h))]
  in
  List.rev (loop liste_entiers [])

let select table str_liste =
  let rec loop enrg acc =
    match enrg with
    | [] -> acc
    | h::t -> loop t acc@[(proj (indexes str_liste table.names)) h]
  in
  loop table.records []
     
let satisfy (tbl:table) (c: string * (string -> bool)) (r: string array) : bool =
  let (n,p) = c in
  (*Array.fold_left (fun acc nom -> if p (index nom tbl.names)*)
  

let find_one table critere =
  let rec loop enrg =
    match enrg with
    | [] -> raise Not_found
    | h::t -> if satisfy table critere h then h
              else loop t
  in
  loop table.records

let update (tbl:table) (c:string * (string -> bool)) (n:string) (v:string) : unit =
  assert false 

let find_all table critere =
  let rec loop enrg acc =
    match enrg with
    | [] -> acc
    | h::t -> if satisfy table critere h then acc@[h]
              else loop t acc
  in
  loop table.records
  
let find_all2 table liste_critere =
  let rec loop liste_critere acc =
    match liste_critere with
    | [] -> acc
    | h::t -> loop t acc@[find_all table h]
  in
  loop liste_critere [] 
