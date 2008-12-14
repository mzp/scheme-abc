open Base

type name = QName of string * string | SName of string
type scope = Global | Scope of int
type bind = Register of int | Slot of scope * int | Member of scope * string
type env  = {depth: int; binding: (name * bind) list}

type 'expr expr_type =
    [ 'expr Ast.expr_type
    | `BindVar of bind Node.t]
type expr =
    expr expr_type

let empty = {
  depth  =0;
  binding=[]
}

let get_bind x {binding=binding} =
  (maybe @@ List.assoc x) binding

let let_env {depth=n; binding=binding} vars =
  {depth  = n+1;
   binding=
      List.map (fun ({Node.value = var},_) ->
		  let bind =
		    Member (Scope n,var) in
		    (SName var,bind)) vars @ binding}

let rec trans (env : env) (expr : Ast.expr) : expr =
  match expr with
      `Int _    | `String _  | `Bool _   | `Float _ as e ->
	e
    | `Var ({Node.value=(x,y)} as loc) ->
	let name =
	  if x = "" then SName y else QName (x,y) in
	  begin match get_bind name env with
	      Some q ->
		`BindVar {loc with Node.value = q }
	    | None ->
		`Var loc
	  end
    | `Lambda (args,body) ->
	let args' =
	  ExtList.List.mapi (fun i {Node.value = arg}->
			       (SName arg,Register (i+1)))
	    args in
	  `Lambda (args,trans {empty with binding = args'} body)
    | `Let (decls,body) ->
	let env' =
	  let_env env decls in
	  `Let (List.map (Tuple.T2.map2 @@ trans env) decls,trans env' body)
    | `LetRec (decls,body) ->
	let env' =
	  let_env env decls in
	  `Let (List.map (Tuple.T2.map2 @@ trans env') decls,trans env' body)
    | `Call exprs ->
	`Call (List.map (trans env) exprs)
    | `If (a,b,c) ->
	`If (trans env a,trans env b,trans env c)
    | `Block exprs ->
	`Block (List.map (trans env) exprs)
    | `New (klass,args) ->
	`New (klass,List.map (trans env) args)
    | `Invoke (obj,name,args) ->
	`Invoke (trans env obj,name,List.map (trans env) args)
    | `SlotRef (obj,name) ->
	`SlotRef (trans env obj,name)
    | `SlotSet (obj,name,value) ->
	`SlotSet (trans env obj,name,trans env value)
