open Base
open Asm
open Ast
open Util
open Cpool

let empty_method = {
  name="";
  params=[];
  return=0;
  flags=0;
  instructions=[];
  traits=[];
  exceptions=[]}

test call =
    assert_equal [{empty_method with
		     instructions=[
		       GetLocal;
		       PushScope;
		       FindPropStrict (QName ((Namespace ""),"print"));
		       PushString "Hello";
		       CallPropLex (QName ((Namespace ""),"print"),1);
		       Pop;
		       ReturnVoid ]}] @@
      generate_method (Method ("",[Call ("print",[String "Hello"])]))

test int = assert_equal [{empty_method with 
			    instructions=[
			      GetLocal;
			      PushScope;
			      PushInt 30;
			      ReturnVoid]}] @@
  generate_method (Method ("",[Int 30]))
