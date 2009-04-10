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

type 'expr stmt_type =
    [ 'expr Ast.stmt_type
    | `ReDefine of Ast.stmt_name * int * 'expr]

let fold f g fold_rec env =
  function
      #Ast.expr_type as e ->
	Ast.fold f g fold_rec env e
    | `BindVar _ as e ->
	g (f env e) e

let fold_stmt f g env =
  function
      #Ast.stmt_type as s ->
	Ast.fold_stmt f g env s
    | `Rdefine _ as s ->
	g (f env s) s

let lift f =
  function
      #Ast.stmt_type as s ->
	Ast.lift f s
    | `ReDefine (name,slot,expr) ->
	`ReDefine (name, slot, f expr)

type expr =
    expr expr_type

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

let rec fold' f g env expr =
  fold f g (fold' f g) env expr

let trans_expr env (expr : Ast.expr) : expr =
  expr +> fold'
    begin fun env expr ->
       match expr with
	 | `Lambda (args,body) ->
	     let args' =
	       ExtList.List.mapi (fun i {Node.value = arg}->
				    (sname arg,Register (i+1)))
		 args in
	       {empty with binding = args'}
	 | `Let (decls,_) | `LetRec (decls,_) ->
	     let_env env decls
	 | _ ->
	     env
    end
    begin fun env expr ->
       match expr with
	 `Var ({Node.value=name} as loc) ->
	   begin match get_bind name env with
	       Some q ->
		 `BindVar {loc with Node.value = q }
	     | None ->
		 `Var loc
	   end
	 | _ ->
	     expr
    end env

let trans_method ({Ast.args=args; body=body} as m) =
  let args' =
    ExtList.List.mapi (fun i {Node.value = arg}->
			 (sname arg,Register (i)))
      args in
    { m with
	Ast.body = trans_expr {empty with binding = args'} body
    }

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
		    binding=(qname,Slot (Global,id))::bs;
		} in
		  env',`ReDefine (name,n-1,trans_expr env' expr)
	    | None ->
		let id =
		  1 + slot_count in
		let env' = {
		  env with
		    slot_count = slot_count + 1;
		    slots   = (qname,id)::slots;
		    binding=(qname,Slot (Scope (n-1),id))::bs;
		} in
		  env',`ReDefine (name,n-1,trans_expr env' expr)
	    | Some (Slot (_,id)) ->
		let env' = {
		  env with
		    depth  = n+1;
		    binding=(qname,Slot (Scope n,id))::bs
		} in
		  env',`Define (name,trans_expr env' expr)
	    | Some _ ->
		failwith "must not happen"
	  end
    | `Expr expr ->
	env,`Expr (trans_expr env expr)
    | `Class ({Ast.class_name=name; methods=methods} as c) ->
	let qname =
	  to_qname name in
	let env' = {
	  env with
	    binding    = (qname,(Member (Global,qname)))::bs
	} in
	  env',`Class {
	    c with
	      Ast.methods = List.map trans_method methods
	  }

let slots_of_env {slots = slots} =
  slots

let trans program =
  let (env,program') =
    map_accum_left trans_stmt {empty with depth=1} program in
    List.rev (slots_of_env env),program'
