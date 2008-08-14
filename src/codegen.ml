open Base
open Ast
open Asm
open Cpool

type bind = Scope of int  | Register of int
type env  = {depth:int; binding: (string * bind) list }

let empty_env =
  {depth=0; binding=[]}

let add_scope names {depth=n;binding=xs} =
  let names' =
    List.map (fun name-> name,Scope n) names in
    {depth=n+1; binding=names' @ xs}

let add_current_scope name {depth=n;binding=xs} =
    {depth=n; binding=(name,Scope (n-1))::xs}

let add_register names env =
  let names' = 
    ExtList.List.mapi (fun i name-> name,Register (i+1)) names in
    {env with binding = names'@env.binding}

let get_bind name {binding=xs} =
  List.assoc name xs

let get_bind_sure name state =
  try
    Some (get_bind name state)
  with Not_found ->
    None

let is_bind name {binding=xs} =
  List.mem_assoc name xs

let ensure_scope name env =
  match get_bind name env with
      Scope x -> 
	x
    | _ ->
	failwith ("scope not found:"^name)


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
    | Lambda (args,body) ->
	let env' =
	  add_register args empty_env in
	let args' =
	  List.map (const 0) args in
	let m =
	  Asm.make_meth ~args:args' "" @@ generate_expr body env' in
	  [NewFunction m]
    | Class (name,sname,xs) ->
	let methods =
	  List.map (fun (name,args,body)-> 
		      match gen @@ Lambda (args,body) with
			  [NewFunction m] -> (name,m)
			| _ -> failwith "must not happen") xs in
	let init = 
	  List.assoc "init" methods in
	let klass = {
	  Asm.cname = make_qname name;
	  sname     = make_qname sname;
	  flags_k   = [];
	  cinit     = make_meth "init" [];
	  iinit     = init;
	  interface = [];
	  methods   = List.map snd methods;
	} in
	  [NewClass klass]
    | Var name ->
	let qname = 
	  make_qname name in
	  begin match get_bind_sure name env with
	      Some (Scope scope) ->
		[GetScopeObject scope;
		 GetProperty qname]
	    | Some (Register n) ->
		[GetLocal n]
	    | _ ->
		[GetLex qname]
	  end
    | Let (vars,body) ->
	let env' =
	  add_scope (List.map fst vars) env in
	let inits =
	  HList.concat_map (fun (name,init)-> 
		       List.concat [[PushString name];gen init]) vars in
	  List.concat [inits;
		       [NewObject (List.length vars);
			PushWith];
		       generate_expr body env';
		       [PopScope]]
    | LetRec (vars,body) ->
	let env' =
	  add_scope (List.map fst vars) env in
	let init = 
	  HList.concat_map (fun (name,init)->
			List.concat [[GetScopeObject (ensure_scope name env')];
				     gen init;
				     [SetProperty (make_qname name)]])
	    vars in
	  List.concat [[NewObject 0;PushWith];
		       init;
		       generate_expr body env';
		       [PopScope]]
    | Ast.Call (Var name::args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    HList.concat_map gen args;
	    [inst]]
    | Ast.Call (Var name::args) ->
	let qname =
	  make_qname name in
	let nargs =
	  List.length args in
	let args' =
	  HList.concat_map gen args; in
	  begin match get_bind_sure name env with
	      Some (Scope scope) ->
		List.concat [[GetScopeObject scope];
			     args';
			     [CallPropLex (make_qname name,nargs)]]
	    | Some (Register n) ->
		List.concat [[GetLocal n;
			      GetGlobalScope];
			     args';
			     [Asm.Call nargs]]
	    | _ ->
		List.concat [[FindPropStrict qname];
			     args';
			     [CallPropLex (qname,nargs)]] 
	  end
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

let generate_stmt env stmt =
  match stmt with
      Expr expr -> 
	env,generate_expr expr env
    | Define (name,body) when not @@ is_bind name env ->
	let env' = 
	  add_current_scope name env in
	let scope = 
	  ensure_scope name env' in
	let body' =
	  List.concat [generate_expr body env';
		       [GetScopeObject scope;
			Swap;
			SetProperty (make_qname name)]] in
	  env',body'
    | Define (name,body) ->
	let env' = 
	  add_scope [name] env in
	let scope = 
	  ensure_scope name env' in
	let body' =
	  List.concat [[NewObject 0;PushWith];
		       generate_expr body env';
		       [GetScopeObject scope;
			Swap;
			SetProperty (make_qname name)]] in
	  env',body'

let generate_program xs env =
  List.concat @@ snd @@ map_accum_left generate_stmt env xs

let generate_method xs =
  let init_env =
    add_scope ["this"] empty_env in
  let program =
    generate_program xs init_env in
    Asm.make_meth "" ([GetLocal_0;PushScope] @ program)

let generate program =
  let m = 
    generate_method @@ Closuretrans.trans program in
  let {Asm.abc_cpool=cpool;
       method_info=info;
       method_body=body} =
    assemble m in
    { Abc.cpool=cpool;
      Abc.method_info=info;
      Abc.method_body=body;
      Abc.metadata=[]; Abc.classes=[]; Abc.instances=[];
      Abc.script=[{Abc.init=0; trait_s=[] }] }

