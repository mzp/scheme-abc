open Base

(* env *)
type name = QName of string * string | SName of string
type scope = Global | Scope of int
type bind = Register of int | Slot of scope * int | Member of scope * string
type env  = {depth: int; binding: (name * bind) list}

(* new ast *)
type 'expr expr_type =
    [ 'expr Ast.expr_type
    | `BindVar of bind Node.t]
type expr =
    expr expr_type
type stmt =
    expr Ast.stmt_type


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

let rec trans_expr (env : env) (expr : Ast.expr) : expr =
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
	  `Lambda (args,trans_expr {empty with binding = args'} body)
    | `Let (decls,body) ->
	let env' =
	  let_env env decls in
	  `Let (List.map (Tuple.T2.map2 @@ trans_expr env) decls,trans_expr env' body)
    | `LetRec (decls,body) ->
	let env' =
	  let_env env decls in
	  `Let (List.map (Tuple.T2.map2 @@ trans_expr env') decls,trans_expr env' body)
    | `Call exprs ->
	`Call (List.map (trans_expr env) exprs)
    | `If (a,b,c) ->
	`If (trans_expr env a,trans_expr env b,trans_expr env c)
    | `Block exprs ->
	`Block (List.map (trans_expr env) exprs)
    | `New (klass,args) ->
	`New (klass,List.map (trans_expr env) args)
    | `Invoke (obj,name,args) ->
	`Invoke (trans_expr env obj,name,List.map (trans_expr env) args)
    | `SlotRef (obj,name) ->
	`SlotRef (trans_expr env obj,name)
    | `SlotSet (obj,name,value) ->
	`SlotSet (trans_expr env obj,name,trans_expr env value)

let trans_stmt ({depth=n;binding=xs} as env) : Ast.stmt -> env * stmt =
  function
      `Define (name,expr) ->
	let qname =
	  match name with
	      `Public {Node.value=(ns,name)} | `Internal {Node.value=(ns,name)} ->
		QName (ns,name) in
	let env' =
	  {env with
	     binding=(qname,Member (Scope (n-1),""))::xs} in
	  env',`Define (name,trans_expr env' expr)
    | `Expr expr ->
	env,`Expr (trans_expr env expr)
    | `Class _ ->
	failwith "not yet"

let trans =
  snd $ map_accum_left trans_stmt empty



