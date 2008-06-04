open Base
open Asm

(** expression has no side-effect. *)
type expr = 
    Lambda of string list * expr
  | Call of string * expr list
  | String of string
  | Int of int
  | If of expr * expr * expr
  | Let of (string*expr) list * expr
  | Var of string
  | Block of expr list

(** statement has side-effect *)
type stmt = 
  | Define of string * expr
  | Expr of expr

type program = stmt list

(** {6 Environment function} *)
type bind = Scope of int * int | Register of int
type env  = int * (string * bind) list

let empty_env =
  (0,[])

let add_scope names ((scope,env) : env) =
  let scope' =
    scope + 1 in
  let names' =
    ExtList.List.mapi (fun i name-> name,Scope (scope',i)) names in
    scope',names' @ env

let add_current_scope name ((scope,env) : env) =
  let i =
    try
      fst @@ ExtList.List.findi (fun _ (_,x) -> match x with Scope _ -> true | _ -> false) env 
    with Not_found ->
      -1 in
    scope,(name,Scope (scope,i+1))::env


let add_register names ((scope,env) : env) =
  let names' = 
    ExtList.List.mapi (fun i name-> name,Register (i+1)) names in
    scope,names' @ env

let get_bind name (_,env) =
  List.assoc name env

let is_bind name (_,env) =
  List.mem_assoc name env

let ensure_scope name env =
  match get_bind name env with
      Scope (x,y) -> 
	x,y
    | _ ->
	failwith ("scope not found:"^name)

(** {6 Utility} *)
let make_qname x = 
  Cpool.QName ((Cpool.Namespace ""),x)

let make_meth ?(args=[]) name body = 
  let inst =
    [GetLocal_0;PushScope] @
      body @
      [ReturnValue] in
  { name  = name;
    params= args;
    return=0;
    flags =0;
    exceptions=[];
    traits=[];
    instructions=inst}

(* bulitin *)
let builtin = ["+",(Add_i,2);
	       "-",(Subtract_i,2);
	       "*",(Multiply_i,2);
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
    | String str -> [PushString str]
    | Int n -> [PushInt n]
    | Lambda (args,body) ->
	let env' =
	  add_register args env in
	let args' =
	  List.map (const 0) args in
	let m = 
	  make_meth ~args:args' "" @@ generate_expr body env' in
	  [NewFunction m]
    | Block xs ->
	List.concat @@ interperse [Pop] @@ (List.map gen xs)
    | Var name ->
	begin match get_bind name env with
	    Scope (scope,index) ->
	      let qname = 
		make_qname @@ string_of_int index in
		[GetScopeObject scope;
		 GetProperty qname]
	  | Register n ->
	      [GetLocal n]
	end
    | Let (vars,body) ->
	let env' =
	  add_scope (List.map fst vars) env in
	let inits =
	  concat_map (fun (name,init)-> 
		       List.concat [gen init]) vars in
	  List.concat [inits;
		       [NewArray (List.length vars);
			PushScope];
		       generate_expr body env';
		       [PopScope]]
    | Call (name,args) when is_builtin name args ->
	let inst,_ =
	  List.assoc name builtin in
	  List.concat [
	    concat_map gen args;
	    [inst]]
    | Call (name,args) when is_bind name env ->
	let nargs =
	  List.length args in
	begin match get_bind name env with
	    Scope (scope,index) ->
	      List.concat [[GetScopeObject scope];
			   concatMap gen args;
			   [CallPropLex (make_qname @@ string_of_int index,nargs)]]
	  | Register n ->
	      List.concat [[GetLocal n;
			    GetGlobalScope];
			     concatMap gen args;
			   [Asm.Call nargs]]
	end
    | Call (name,args) ->
	let qname =
	  make_qname name in
	let nargs = 
	  List.length args in
	  List.concat [[FindPropStrict qname];
		       concatMap gen args;
		       [CallPropLex (qname,nargs)]]
    | If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if = 
	  Label.make () in
	let prefix = List.concat @@ match cond with
	    Call ("=" ,[a;b]) ->
	      [gen a;gen b;[IfNe l_alt]]
	  | Call (">" ,[a;b]) ->
	      [gen a;gen b;[IfNgt l_alt]]
	  | Call (">=",[a;b]) ->
	      [gen a;gen b;[IfNge l_alt]]
	  | Call ("<" ,[a;b]) ->
	      [gen a;gen b;[IfNlt l_alt]]
	  | Call ("<=",[a;b]) ->
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
    | Define (name,body) -> 	
	let env' =
	  add_current_scope name env in
	let scope,index =
	  ensure_scope name env' in
	let body' =
	  (generate_expr body env)@
	    [GetScopeObject scope;
	     Swap;
	     SetProperty (make_qname @@ string_of_int index)] in
	  env',body'

let generate_program xs =
  List.concat @@ snd @@ map_accum_left generate_stmt empty_env xs

let generate_method xs =
  make_meth "" (generate_program xs)

let generate program =
  let m = 
    generate_method program in
  let cpool,info,body =
    assemble m in
    { Abc.cpool=cpool;
      Abc.method_info=info;
      Abc.method_body=body;
      Abc.metadata=[]; Abc.classes=[]; Abc.instances=[];
      Abc.script=[{Abc.init=0; trait_s=[] }] }
