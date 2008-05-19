open Base
open Asm

type ast = 
    Method of string * ast list 
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


let rec generate_expr ast = 
  let expr ast =
    right (generate_expr ast) in
  let binary_op op l r =
    Right ((expr l)@(expr r)@[op])  in
  match ast with
    | String str -> Right [PushString str]
    | Int n -> Right [PushInt n]
    | Add (l,r) -> binary_op Add_i l r
    | Sub (l,r) -> binary_op Subtract_i l r
    | Mul (l,r) -> binary_op Multiply_i l r
    | Div(l,r)  -> binary_op Divide l r
    | Eq (l,r)  -> binary_op Equals l r
    | Gt (l,r)  -> binary_op GreaterThan l r
    | Geq (l,r) -> binary_op GreaterEquals l r
    | Lt (l,r)  -> binary_op LessThan l r
    | Leq (l,r) -> binary_op LessEquals l r
    | Method (name,body) -> 
	let inst = 
	  [GetLocal;PushScope]
	  @ (concatMap expr body)
	  @ [ReturnVoid] in
	  Left [{ name  = name;
		  params=[];
		  return=0;
		  flags =0;
		  exceptions=[];
		  traits=[];
		  instructions=inst}]
    | Call (name,args) ->
	let mname =
	  Cpool.QName ((Cpool.Namespace ""),name) in
	Right ([FindPropStrict mname]
	       @ (concatMap expr args)
	       @ [CallPropLex (mname,List.length args);
		  Pop;])
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
	  Right (List.concat [prefix;
			      expr cons;
			      [Jump l_if;Label l_alt];
			      expr alt;
			      [Label l_if]])


let generate_method program =
    left @@ generate_expr program

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

