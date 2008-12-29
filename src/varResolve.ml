open Base

(* env *)
type name = string * string
type scope = Scope of int | Global
type bind =
    Register of int
  | Slot of scope * int
  | Member of scope * name

type slot = name * int

type env  = {
  depth: int;
  binding: (name * bind) list;
  slots: slot list;
  slot_count : int
}

(* new ast *)
type 'expr expr_type =
    [ 'expr Ast.expr_type
    | `BindVar of bind Node.t]
type expr =
    expr expr_type
type 'expr stmt_type =
    [ 'expr Ast.stmt_type
    | `ReDefine of Ast.stmt_name * int * 'expr]

type stmt =
    expr stmt_type

type program =
    stmt list

let empty = {
  depth   = 0;
  binding = [];
  slots   = [];
  slot_count = 0
}

let get_bind x {binding=binding} =
  (maybe @@ List.assoc x) binding

let sname x =
  ("",x)

let let_env ({depth=n; binding=binding} as env) vars =
  {env with
     depth  = n+1;
     binding=
      List.map (fun ({Node.value = var},_) ->
		  let bind =
		    Member (Scope n,("",var)) in
		    (sname var,bind)) vars @ binding}

let rec trans_expr env (expr : Ast.expr) : expr =
  match expr with
      `Int _    | `String _  | `Bool _   | `Float _ as e ->
	e
    | `Var ({Node.value=name} as loc) ->
	begin match get_bind name env with
	    Some q ->
	      `BindVar {loc with Node.value = q }
	  | None ->
	      `Var loc
	end
    | `Lambda (args,body) ->
	let args' =
	  ExtList.List.mapi (fun i {Node.value = arg}->
			       (sname arg,Register (i+1)))
	    args in
	  `Lambda (args,trans_expr {empty with binding = args'} body)
    | `Let (decls,body) ->
	let env' =
	  let_env env decls in
	  `Let (List.map (Tuple.T2.map2 @@ trans_expr env) decls,trans_expr env' body)
    | `LetRec (decls,body) ->
	let env' =
	  let_env env decls in
	  `LetRec (List.map (Tuple.T2.map2 @@ trans_expr env') decls,trans_expr env' body)
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

let trans_method (name,args,body) =
  let args' =
    ExtList.List.mapi (fun i {Node.value = arg}->
			 (sname arg,Register (i)))
      args in
    (name,args,trans_expr {empty with binding = args'} body)

let to_qname =
  function
      `Public {Node.value=name} | `Internal {Node.value=name} ->
	name

let trans_stmt ({depth=n; binding=bs; slots=slots; slot_count = slot_count } as env) :
    Ast.stmt -> env * stmt =
  function
      `Define (name,expr) ->
	let qname =
	  to_qname name in
	  begin match get_bind qname env with
	      None when n = 1 ->
		let id =
		  1 + slot_count in
		let env' = {
		  env with
		    slot_count = slot_count + 1;
		    slots   = (qname,id)::slots;
		    binding=(qname,Member (Global,qname))::bs;
		} in
		  env',`ReDefine (name,n-1,trans_expr env' expr)
	    | None ->
		let env' = {
		  env with
		    binding=(qname,Member (Scope (n-1),qname))::bs;
		} in
		  env',`ReDefine (name,n-1,trans_expr env' expr)
	    | Some _ ->
		let env' = {
		  env with
		    depth  = n+1;
		    binding=(qname,Member (Scope n,qname))::bs
		} in
		  env',`Define (name,trans_expr env' expr)
	  end
    | `Expr expr ->
	env,`Expr (trans_expr env expr)
    | `Class (name,super,attrs,methods) ->
	let qname =
	  to_qname name in
	let env' = {
	  env with
	    binding    = (qname,(Member (Global,qname)))::bs
	} in
	  env',`Class (name,super,attrs,List.map trans_method methods)

let slots_of_env {slots = slots} =
  slots

let trans program =
  let (env,program') =
    map_accum_left trans_stmt {empty with depth=1} program in
    List.rev (slots_of_env env),program'
