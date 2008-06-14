open Base
open Asm
open Ast
open Util
open Cpool

let result ?(args=[]) ?(prefix=[GetLocal_0; PushScope]) inst = {
  name="";
  params=args;
  return=0;
  flags=0;
  instructions=prefix@inst@[ReturnValue];
  traits=[];
  exceptions=[]}

let compile x =
  (generate_method [Expr x])

test call =
    assert_equal 
      (result [FindPropStrict (QName ((Namespace ""),"print"));
	       PushString "Hello";
	       CallPropLex (QName ((Namespace ""),"print"),1)])
      (compile (Call ("print",[String "Hello"])))

test int = 
  assert_equal 
    (result [PushInt 30])
    (compile (Int 30))


test add = 
  assert_equal
    (result [PushInt 1;PushInt 2;Add_i;])
    (compile (Call ("+",[Int 1;Int 2])))

test boolean = 
  assert_equal
    (result [PushInt 1;PushInt 2;Equals])
    (compile (Call ("=",[Int 1;Int 2])))

test block =
  assert_equal
    (result [PushInt 1;Pop;PushInt 2])
    (compile (Block [Int 1;Int 2]))

test if_ =
  let a =
    Label.peek 0 in
  let b = Label.peek 1 in
  assert_equal
    (result [PushInt 10; PushInt 20;  
	     IfNe a; PushInt 0; Jump b;
	     Label a;PushInt 1; Label b])
    (compile (If ((Call ("=",[Int 10;Int 20])),Int 0,Int 1)))

test let_ =
  assert_equal
    (result [PushString "x"; PushInt 1;
	     PushString "y"; PushInt 2;
	     NewObject 2;
	     PushWith;
	     GetScopeObject 1;
	     GetProperty (QName ((Namespace ""),"x"));
	     Pop;
	     GetScopeObject 1;
	     GetProperty (QName ((Namespace ""),"y"));
	     PopScope])
    (compile (Let (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"])))

test call =
  assert_equal 
    (result [NewFunction (result ~prefix:[] [PushInt 42]) ])
    (compile (Lambda ([],Block [Int 42])))

test call_with_args =
  assert_equal 
    (result [NewFunction (result ~args:[0;0] ~prefix:[] [GetLocal 2])])
    (compile (Lambda (["x";"y"],Block [Var "y"])))
