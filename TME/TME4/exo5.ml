type value = Integer of int | Boolean of bool

type expr = Add of expr * expr 
	    | Or of expr * expr 
	    | Val of value 
	    | If of expr * expr * expr
	    | Var of string
	    | Assign of string *expr

type env = (string * value) list


let rec eval (e:expr) (env:env) :(value*env) = 
  match e with 
  | Val v -> v,env
  | Add (expr1, expr2) ->
     begin
       let env1,expr1 = eval expr1 env in
       let env1,expr2 = eval expr2 env in
       match (eval expr1 v1, eval expr2 v2) with
	   | (Integer i1, v1),(Integer i2, v2) -> (Integer (i1+i2), env)
	   | _ -> raise (Invalid_argument ("eval"))
     end
       
  | If (expr1, expr2, expr3) ->
     begin
       match (eval expr1 env) with
	   | Boolean true,env -> eval expr2 env
	   | Boolean false,env -> eval expr3 env
	   | _ -> raise (Invalid_argument ("eval"))
     end
       
  | Or (expr1, expr2) ->
     begin
       match (eval expr1 env, eval expr2 env) with
	   | (Boolean b1,env) , (Boolean b2, env) -> (Boolean (b1 || b2),env)
	   | _ -> raise (Invalid_argument ("eval"))
     end
       
  | Var s -> (List.assoc s env),env
  | Assign (exp, s) -> 
			
