open Base
open Asm

type ast = 
    Method of string * ast list 
  | Call of string * ast list
  | String of string
  | Int of int
  | Add of ast * ast

let rec expr x=
  right (generate_expr x)
and generate_expr = 
  function 
      Method (name,body) -> 
	let inst = 
	  [GetLocal;PushScope]
	  @ (concatMap expr body)
	  @ [ReturnVoid]
	in
	  Left [{ name  = name;
		  params=[];
		  return=0;
		  flags =0;
		  exceptions=[];
		  traits=[];
		  instructions=inst}]
    | String str ->
	Right [PushString str]
    | Int n ->
	Right [PushInt n]
    | Add (l,r) ->
	Right ((expr l)@(expr r)@[Add_i])
    | Call (name,args) ->
	let mname =
	  Cpool.QName ((Cpool.Namespace ""),name) in
	Right ([FindPropStrict mname]
	       @ (concatMap expr args)
	       @ [CallPropLex (mname,List.length args);
		  Pop;])

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

