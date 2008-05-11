open Base
open Asm
open Ast
open Util

test call =
    assert_equal [{
      name="";
      params=[];
      return=0;
      flags=0;
      instructions=[
	GetLocal;
	PushScope;
	FindPropStrict (QName ((Namespace ""),"print"));
	PushString "Hello";
	CallPropLex (QName ((Namespace ""),"print"),1);
	Pop;
	ReturnVoid
      ];
      traits=[];
      exceptions=[]}] @@ 
      generate_method (Method ("",[Call ("print",[String "Hello"])]))
