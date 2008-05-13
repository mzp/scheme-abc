open Base
open Asm
open Ast
open Util
open Cpool

let make inst = [{
  name="";
  params=[];
  return=0;
  flags=0;
  instructions=inst;
  traits=[];
  exceptions=[]}]

test call =
    assert_equal (make [GetLocal;
			PushScope;
			FindPropStrict (QName ((Namespace ""),"print"));
			PushString "Hello";
			CallPropLex (QName ((Namespace ""),"print"),1);
			Pop;
			ReturnVoid ]) @@
      generate_method (Method ("",[Call ("print",[String "Hello"])]))

test int = assert_equal (make [GetLocal;
			       PushScope;
			       PushInt 30;
			       ReturnVoid]) @@
  generate_method (Method ("",[Int 30]))


test add = assert_equal (make [GetLocal;
			       PushScope;
			       PushInt 1;
			       PushInt 2;
			       Add_i;
			       ReturnVoid]) 
  (generate_method (Method ("",[Add (Int 1,Int 2)])))
