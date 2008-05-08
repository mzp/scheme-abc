open Base
open Asm

type ast = 
    Method of string * ast list 
  | Call of string * ast list
  | String of string

let rec generate_expr = 
  function 
      Method (name,expr) -> 
	let inst = 
	  [GetLocal;PushScope]
	  @ (concatMap (right$generate_expr) expr)
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
    | Call (name,args) ->
	let mname =
	  QName ((Asm.Namespace ""),name) in
	Right ([FindPropStrict mname]
	       @ (concatMap (right$generate_expr) args)
	       @ [CallPropLex (mname,List.length args);
		  Pop;])

let generate program =
  let m = 
    left @@ generate_expr (Method ("",program)) in
  let cpool,info,body =
    assemble m in
    { Abc.cpool=cpool;
      Abc.method_info=info;
      Abc.method_body=body;
      Abc.metadata=[]; Abc.classes=[]; Abc.instances=[];
      Abc.script=[{Abc.init=0; trait_s=[] }] }

let test () = 
  let abc = generate [Call ("print",[String "Hello,";String "world!!"])] in
  let ch = open_out_bin "emitter.abc" in
    Bytes.output_bytes ch @@ Abc.bytes_of_abc abc;
    close_out ch;
    abc
