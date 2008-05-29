open Base
open Asm

type ast = 
    Method of string * string list * ast
  | Call of string * ast list
  | String of string
  | Int of int
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast  
  | Div of ast * ast
  | Eq of ast * ast
  | Lt of ast * ast
  | Leq of ast * ast
  | Gt of ast * ast
  | Geq of ast * ast
  | If of ast * ast * ast
  | Let of (string*ast) list * ast
  | Var of string
  | Block of ast list

type bind = Scope of int * int | Register of int
type env  = int * (string * bind) list

let empty_env =
  0,[]

let add_scope names (scope,env) =
  let scope' =
    scope + 1 in
    scope',ExtList.List.mapi (fun i name-> name,Scope (scope',i)) names @ env

let add_register names (scope,env) =
  scope,ExtList.List.mapi (fun i name-> name,Register (i+1)) names @ env

let get_bind name (_,env) =
  List.assoc name env

let is_bind name (_,env) =
  List.mem_assoc name env

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
    traits=["a",Slot 1];
    instructions=inst}

let rec generate_expr ast env = 
  let expr e =
    generate_expr e env in
  let binary_op op l r =
    ((expr l)@(expr r)@[op])  in
  match ast with
    (* literal *)
    | String str -> [PushString str]
    | Int n -> [PushInt n]
    (* arith *)
    | Add (l,r) -> binary_op Add_i l r
    | Sub (l,r) -> binary_op Subtract_i l r
    | Mul (l,r) -> binary_op Multiply_i l r
    | Div(l,r)  -> binary_op Divide l r
    (* predicate *)
    | Eq (l,r)  -> binary_op Equals l r
    | Gt (l,r)  -> binary_op GreaterThan l r
    | Geq (l,r) -> binary_op GreaterEquals l r
    | Lt (l,r)  -> binary_op LessThan l r
    | Leq (l,r) -> binary_op LessEquals l r
    (* syntax *)
    | Method (name,args,body) ->
	let env' =
	  add_register args env in
	let args' =
	  List.map (const 0) args in
	let m = 
	  make_meth ~args:args' name @@ generate_expr body env' in
	  [NewFunction m]
    | Block xs ->
	List.concat @@ interperse [Pop] @@ (List.map expr xs)
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
	  concatMap (fun (name,init)-> 
		       List.concat [expr init]) vars in
	  List.concat [inits;
		       [NewArray (List.length vars);
			PushScope];
		       generate_expr body env';
		       [PopScope]]
    | Call (name,args) when is_bind name env ->
	let nargs =
	  List.length args in
	begin match get_bind name env with
	    Scope (scope,index) ->
	      List.concat [[GetScopeObject scope];
			   concatMap expr args;
			   [CallPropLex (make_qname @@ string_of_int index,nargs)]]
	  | Register n ->
	      List.concat [[GetLocal n;
			    GetGlobalScope];
			     concatMap expr args;
			   [Asm.Call nargs]]
	end
    | Call (name,args) ->
	let qname =
	  make_qname name in
	let nargs = 
	  List.length args in
	  List.concat [[FindPropStrict qname];
		       concatMap expr args;
		       [CallPropLex (qname,nargs)]]
    | If (cond,cons,alt) ->
	let l_alt =
	  Label.make () in
	let l_if = 
	  Label.make () in
	let prefix = List.concat @@ match cond with
	  | Eq (a,b) ->
	      [expr a;expr b;[IfNe l_alt]]
	  | Gt (a,b) ->
	      [expr a;expr b;[IfNgt l_alt]]
	  | Geq (a,b) ->
	      [expr a;expr b;[IfNge l_alt]]
	  | Lt (a,b) ->
	      [expr a;expr b;[IfNlt l_alt]]
	  | Leq (a,b) ->
	      [expr a;expr b;[IfNle l_alt]]
	  | _ ->
	      [expr cond;[IfFalse l_alt]] in
	  List.concat [prefix;
		       expr cons;
		       [Jump l_if;Label l_alt];
		       expr alt;
		       [Label l_if]]

let generate_method program =
  make_meth "" (generate_expr program empty_env)

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

