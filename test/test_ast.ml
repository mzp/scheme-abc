open Base
open Asm
open Ast
open Util
open Cpool

let compile inst = [{
  name="";
  params=[];
  return=0;
  flags=0;
  instructions=[GetLocal;PushScope;]@inst@[ReturnVoid];
  traits=[];
  exceptions=[]}]

let result x =
  (generate_method (Method ("",[x])))

test call =
    assert_equal 
      (compile [FindPropStrict (QName ((Namespace ""),"print"));
	     PushString "Hello";
	     CallPropLex (QName ((Namespace ""),"print"),1);
	     Pop])
      (result (Call ("print",[String "Hello"])))

test int = 
  assert_equal 
    (compile [PushInt 30])
    (result (Int 30))


test add = 
  assert_equal 
    (compile [PushInt 1;PushInt 2;Add_i;])
    (result (Add (Int 1,Int 2)))

test boolean = 
  assert_equal
    (compile [PushInt 1;PushInt 2;Equals])
    (result ((Eq ((Int 1),(Int 2)))))
