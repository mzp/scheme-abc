open Base
open Ast
open Asm
open Cpool

(* environment *)
type bind = Scope of int | Register of int | Global
type env  = {depth:int; binding: (string * bind) list }

let get_bind name {binding=xs} =
  List.assoc name xs

let get_bind_sure name state =
  try
    Some (get_bind name state)
  with Not_found ->
    None

let is_bind name {binding=xs} =
  List.mem_assoc name xs

let empty_env =
  {depth=0; binding=[("this",Register 0)]}

let script_bootstrap _ =
  [ GetLocal_0; PushScope ],{depth=1; binding=[]}

let arguments args f =
  let b =
    ExtList.List.mapi (fun i arg-> (arg,Register (i+1))) args in
  let args' =
    List.map (const 0) args in
  let code =
    f ({empty_env with binding = b }) args' in
    code

let arguments_self args f =
  let b =
    ExtList.List.mapi (fun i arg-> (arg,Register i)) args in
  let args' =
    List.map (const 0) (List.tl args) in
  let code =
    f ({empty_env with binding = b }) args' in
    code

let let_scope {depth=n; binding=binding} vars f =
  let env' =
    {depth  = n+1;
     binding= List.map (fun (var,_) -> (var,Scope n)) vars @ binding} in
    List.concat [HList.concat_map 
		   (fun (var,init)-> 
		      List.concat [[PushString var]; init]) vars;
		 [NewObject (List.length vars);
		  PushWith];
		 f env';
		 [PopScope]]

let let_rec_scope {depth=n; binding=binding} vars f =
  let env' =
    {depth  = n+1;
     binding= List.map (fun (var,_) -> (var,Scope n)) vars @ binding } in
  let init = 
    HList.concat_map 
      (fun (var,g)->
	 List.concat [[GetScopeObject n];
		      g env';
		      [SetProperty (make_qname var)]]) vars in
    List.concat [[NewObject 0;PushWith];
		 init;
		 f env';
		 [PopScope]]

let define_scope name ({depth=n;binding=xs} as env) f =
  let env' =
    {depth=n; binding=(name,Scope (n-1))::xs} in
  let body' =
    if is_bind name env then
      List.concat [
	[NewObject 0;PushWith];
	f env';
	[GetScopeObject n;
	 Swap;
	 SetProperty (make_qname name)]]
    else
      List.concat [
	f env';
	[GetScopeObject (n-1);
	 Swap;
	 SetProperty (make_qname name)]] in
    env',body'

let define_class name ({sname=super; cname=cname} as klass) env =
  let env' = 
    {env with binding=(name,Global)::env.binding} in
  env',[
    (* init class *)
    GetLex super;
    PushScope;
    GetLex super;
    NewClass klass;
    PopScope;

    (* add to scope *)
    GetGlobalScope;
    Swap;
    InitProperty cname]

let var_ref var env =
  let qname = 
    make_qname var in
    match get_bind_sure var env with
	Some (Scope scope) ->
	  [GetScopeObject scope;
	   GetProperty qname]
      | Some (Register n) ->
	  [GetLocal n]
      | Some Global ->
	  [GetGlobalScope;
	   GetProperty qname]
      | None ->
	  [GetLex qname]

let var_call var args env =
  let qname =
    make_qname var in
  let nargs =
    List.length args in
    match get_bind_sure var env with
	Some (Scope scope) ->
	  List.concat [[GetScopeObject scope];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]
      | Some (Register n) ->
	  List.concat [[GetLocal n;
			GetGlobalScope];
		       List.concat args;
		       [Asm.Call nargs]]
      | Some Global ->
	  List.concat [[GetGlobalScope];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]
      | None ->
	  List.concat [[FindPropStrict qname];
		       List.concat args;
		       [CallPropLex (qname,nargs)]]

(** {6 Builtin operator } *)
let builtin = ["+",(Add_i,2);
	       "-",(Subtract_i,2);
	       "*",(Multiply_i,2);
	       "+.",(Add,2);
	       "-.",(Subtract,2);
	       "*.",(Multiply,2);
	       "/",(Divide,2);
	       "=",(Equals,2);
	       ">",(GreaterThan,2);
	       ">=",(GreaterEquals,2);
	       "<",(LessThan,2);
	       "<=",(LessEquals,2);]

let is_builtin name args =
  try
    let _,n =
      List.assoc name builtin in
      n = List.length args
  with Not_found ->
    false

(** {6 Asm code generation} *)
let rec generate_expr expr env = 
  let gen e =
    generate_expr e env in
  match expr with
    | Bool b ->
	if b then
	  [PushTrue]
	else
	  [PushFalse]
    | Float v->
	[PushDouble v]
    | String str -> [PushString str]
    | Int n when 0 <= n && n <= 0xFF -> [PushByte n]
    | Int n      -> [PushInt n]
    | Block xs   -> List.concat @@ interperse [Pop] @@ (List.map gen xs)
    | New ((ns,name),args) ->
	let qname =
	  make_qname ~ns:ns name in
	List.concat [
	  [FindPropStrict qname];
	  HList.concat_map gen args;
	  [ConstructProp (qname,List.length args)]]
    | Lambda (args,body) ->
	arguments args 
	  (fun e args' ->
	     let body' = 
	       generate_expr body e in
	     let m =
	       Asm.make_meth ~args:args' "" body' in
	       [NewFunction m])
    | Var name ->
	var_ref name env
    | Let (vars,body) ->
	let vars' =
	  List.map (Core.Tuple.T2.map2 ~f:gen) vars in
	  let_scope env vars' @@ generate_expr body
    | LetRec (vars,body) ->
	let vars' =
	  List.map (Core.Tuple.T2.map2 ~f:generate_expr) vars in
	  let_rec_scope env vars' @@ generate_expr body
    | Invoke (obj,name,args)->
	List.concat [
	  gen obj;
	  HList.concat_map gen args;
	  [CallProperty (make_qname name,List.length args)]]
    | Ast.Call (Var name::args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    HList.concat_map gen args;
	    [inst]]
    | Ast.Call (Var name::args) ->
	let args' =
	  List.map gen args in
	  var_call name args' env
    | Ast.Call (name::args) ->
	let nargs =
	  List.length args in
	  List.concat [gen name;
		       [GetGlobalScope];
		       HList.concat_map gen args;
		       [Asm.Call nargs]]
    | Ast.Call [] ->
	failwith "must not happen"
    | If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if = 
	  Label.make () in
	let prefix = List.concat @@ match cond with
	    Ast.Call [Var "=";a;b] ->
	      [gen a;gen b;[IfNe l_alt]]
	  | Ast.Call [Var ">";a;b] ->
	      [gen a;gen b;[IfNgt l_alt]]
	  | Ast.Call [Var ">=";a;b] ->
	      [gen a;gen b;[IfNge l_alt]]
	  | Ast.Call [Var "<";a;b] ->
	      [gen a;gen b;[IfNlt l_alt]]
	  | Ast.Call [Var "<=";a;b] ->
	      [gen a;gen b;[IfNle l_alt]]
	  | _ ->
	      [gen cond;[IfFalse l_alt]] in
	  List.concat [prefix;
		       gen cons;
		       [Jump l_if;Label l_alt];
		       gen alt;
		       [Label l_if]] 


type class_method = {
  cinit: Asm.meth; init: Asm.meth; methods: Asm.meth list
}

let generate_stmt env stmt =
  match stmt with
      Expr expr -> 
	env,(generate_expr expr env)@[Pop]
    | Define (name,body) ->
	define_scope name env @@ generate_expr body
    | Class (name,(ns,sname),_,body) ->
	let name' =
	  make_qname name in
	let sname' = 
	  make_qname ~ns:ns sname in
	let prefix = 
	  [GetLocal_0;
	   ConstructSuper 0] in
	let {init=init; cinit=cinit; methods=methods} =
	  List.fold_left
	    (fun ctx (name,args,body) ->
	       match name with
		   "init" -> 
		     {ctx with init = arguments_self args
			 (fun e args ->
			    Asm.make_proc ~args:args name @@ prefix @ (generate_expr body e))}
		 | "cinit" ->
		     {ctx with cinit = arguments_self args
			 (fun e args ->
			    Asm.make_proc ~args:args name @@ generate_expr body e)}
		 | _       ->
		     {ctx with methods = 
			 (arguments_self args
			    (fun e args->
			       Asm.make_meth ~args:args name @@ generate_expr body e)) :: ctx.methods})
	    {init  = make_proc "init" prefix;
	     cinit = make_proc "cinit" [];
	     methods = []} body in
	let klass = {
	  Asm.cname = name';
	  sname     = sname';
	  flags_k   = [Sealed];
	  cinit     = cinit;
	  iinit     = init;
	  interface = [];
	  methods   = methods
	} in
	  define_class name klass env

let generate_program xs env =
  List.concat @@ snd @@ map_accum_left generate_stmt env xs

let generate_script xs =
  let bootstrap,env =
    script_bootstrap () in
  let program =
    generate_program xs env in
    Asm.make_proc "" (bootstrap @ program)

let generate program =
  let script = 
    generate_script program in
  let {Asm.abc_cpool=cpool;
       method_info=info;
       method_body=body;
       class_info =class_info;
       instance_info=instance_info} =
    assemble script in
  let traits_class =
    ExtList.List.mapi 
      (fun i {Abc.name_i=name} -> 
	 {Abc.t_name=name; data=Abc.ClassTrait (i,i)})
      instance_info in
    { Abc.cpool=cpool;
      method_info=info;
      method_body=body;
      metadata=[]; 
      classes=class_info; 
      instances=instance_info;
      script=[{Abc.init=0; trait_s=traits_class }]}

