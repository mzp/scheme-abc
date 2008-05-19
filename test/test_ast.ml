open Base
open Asm
open Ast
open Util
open Cpool

let result inst = [{
  name="";
  params=[];
  return=0;
  flags=0;
  instructions=[GetLocal_0;PushScope;]@inst@[ReturnVoid];
  traits=[];
  exceptions=[]}]

let compile x =
  (generate_method (Method ("",[x])))

test call =
    assert_equal 
      (result [FindPropStrict (QName ((Namespace ""),"print"));
	     PushString "Hello";
	     CallPropLex (QName ((Namespace ""),"print"),1);
	     Pop])
      (compile (Call ("print",[String "Hello"])))

test int = 
  assert_equal 
    (result [PushInt 30])
    (compile (Int 30))


test add = 
  assert_equal 
    (result [PushInt 1;PushInt 2;Add_i;])
    (compile (Add (Int 1,Int 2)))

test boolean = 
  assert_equal
    (result [PushInt 1;PushInt 2;Equals])
    (compile ((Eq ((Int 1),(Int 2)))))

test if_ =
  let a =
    Label.peek 0 in
  let b = Label.peek 1 in
  assert_equal
    (result [PushInt 10; PushInt 20;  
	     IfNe a; PushInt 0; Jump b;
	     Label a;PushInt 1; Label b])
    (compile (If ((Eq (Int 10,Int 20)),Int 0,Int 1)))

