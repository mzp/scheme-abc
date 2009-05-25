open Base

(* env *)
type name = string list * string
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
type 'expr expr =
    [ 'expr Module.expr
    | `BindVar of bind Node.t]

type ('expr,'stmt) stmt =
    [ ('expr,'stmt) Module.stmt
    | `ReDefine of Module.stmt_name * int * 'expr]

let fold f g fold_rec env =
  function
      `BindVar _ as e ->
	g (f env e) e
    | #Module.expr as e ->
	Module.fold f g fold_rec env e

let fold_stmt f g env =
  function
      `ReDefine _ as s ->
	g (f env s) s
    | #Module.stmt as s ->
	Module.fold_stmt f g env s

let lift f =
  function
      `ReDefine (name,slot,expr) ->
	`ReDefine (name, slot, f expr)
    | #Module.stmt as s ->
	Module.lift f s

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list

(* ------------------------------------------------------------ *)
let empty = {
  depth   = 0;
  binding = [];
  slots   = [];
  slot_count = 0
}

let get_bind x {binding=binding} =
  (maybe @@ List.assoc x) binding

let sname x =
  ([],x)

let let_env env vars =
  let binding =
    vars
    +> List.map (fun ({Node.value = var},_) ->
		   let bind =
		     Member (Scope env.depth,sname var) in
		     (sname var,bind))  in
                     { env with
			 depth   = env.depth + 1;
			 binding = binding @ env.binding }

let bind_var env expr =
  Ast.fix_fold fold
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
    end env expr

let bind_method m =
  open Ast in
  let args' =
    m.args
    +> ExtList.List.mapi
      (fun i { Node.value = arg }->
	 (sname arg,Register i)) in
    { m with
	body = bind_var {empty with binding = args'} m.body
    }

let to_qname =
  function
      `Public {Node.value=name} | `Internal {Node.value=name} ->
	name
let bind_define env (`Define (name,expr)) =
  let qname =
    to_qname name in
    match get_bind qname env with
	None ->
	  let id =
	    1 + env.slot_count in
	  let env' =
	    { env with
		slot_count = id;
		slots      = (qname,id)::env.slots;
		binding    =
	          if env.depth = 1 then
		    (qname,Slot (Global,id))::env.binding
		  else
		    (qname,Slot (Scope (env.depth-1),id))::env.binding
	    } in
	    env',`ReDefine (name,env.depth-1,bind_var env' expr)
      | Some (Slot (_,id)) ->
	  let env' =
	    { env with
		depth  = env.depth + 1;
		binding=(qname,Slot (Scope env.depth,id))::env.binding
	    } in
	    env',`Define (name,bind_var env' expr)
      | Some _ ->
	  failwith "must not happen"

let bind_stmt env =
  function
      `Define _ as s ->
	bind_define env s
    | `Expr expr ->
	env,`Expr (bind_var env expr)
    | `Class c ->
	open Ast in
	let qname =
	  to_qname c.class_name in
	let env' =
	  { env with
	      binding = (qname,(Member (Global,qname)))::env.binding } in
	  env',`Class { c with
			  methods = List.map bind_method c.methods }

let of_module program =
  let { slots = slots}, program' =
    map_accum_left bind_stmt {empty with depth=1} program in
    List.rev slots,program'
