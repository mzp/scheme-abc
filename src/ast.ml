open Base
open Asm

type ast = 
    Method of string * ast
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

let scope_depth = function
    [] -> 0
  | (_,(scope,index))::_ -> scope

let make_qname x = 
  Cpool.QName ((Cpool.Namespace ""),x)

let make_meth name body = 
  let inst =
    [GetLocal_0;PushScope] @
      body @
      [ReturnValue] in
  { name  = name;
    params=[];
    return=0;
    flags =0;
    exceptions=[];
    traits=[];
    instructions=inst}

let rec generate_expr ast table = 
  let expr e =
    generate_expr e table in
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
    | Method (name,body) ->
	let m = 
	  make_meth name @@ expr body in
	  [NewFunction m]
    | Block xs ->
	List.concat @@ interperse [Pop] @@ (List.map expr xs)
    | Var name ->
	let scope,index = 
	  List.assoc name table in
	let qname = 
	  make_qname @@ string_of_int index in
	  [GetScopeObject scope;
	   GetProperty qname]
    | Let (vars,body) ->
	let depth = 
	  scope_depth table + 1 in
	let table' =
	  (ExtList.List.mapi (fun i (name,_) -> name,(depth,i)) vars)@table in
	let inits =
	  concatMap (fun (name,init)-> 
		       List.concat [expr init]) vars in
	  List.concat [inits;
		       [NewArray (List.length vars);
			PushScope];
		       generate_expr body table';
		       [PopScope]]
    | Call (name,args) ->
	let load,qname =
	  if List.mem_assoc name table then
	    let scope,index = 
	      List.assoc name table in
	      GetScopeObject scope,make_qname @@ string_of_int index
	  else
	    let qname =
	      make_qname name in
	      FindPropStrict qname,qname in
	  List.concat [
	    [load];
	    concatMap expr args;
	    [CallPropLex (qname,List.length args)]]
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
  make_meth "" (generate_expr program [])

let generate program =
  let m = 
    generate_method (Method ("",program)) in
  let cpool,info,body =
    assemble m in
    { Abc.cpool=cpool;
      Abc.method_info=info;
      Abc.method_body=body;
      Abc.metadata=[]; Abc.classes=[]; Abc.instances=[];
      Abc.script=[{Abc.init=0; trait_s=[] }] }

