(*listes chainées je connais le principe en C mais en Ocaml c'est probablement différent*)
(*page 30 du cours*)

type 'a cell : {
	mutable elt : 'a ;
	mutable next : 'a clist
}
and 'a clist : Nil | Cons 'a cell

Cons { elt = 1; next = Cons { elt = 2; next = Cons { elt = 3; next = Nil}}}

let rec mapset f xs = 
	match xs with 
	| Nil -> () 
	| Cons c -> (c.elt <- f c.elt; mapset f c.next)