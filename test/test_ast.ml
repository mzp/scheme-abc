open Base
open Asm
open Ast
(*open Util*)
open Cpool

let string_of_insts xs =
  let ys =
    String.concat "; " @@ List.map string_of_instruction xs in
    Printf.sprintf "[ %s ]\n" ys

let assert_equal lhs rhs =
  OUnit.assert_equal ~printer:Std.dump ~msg:"name"
    lhs.name         rhs.name;
  OUnit.assert_equal ~printer:Std.dump ~msg:"params"
    lhs.params       rhs.params;
  OUnit.assert_equal ~printer:Std.dump ~msg:"return"
    lhs.return       rhs.return;
  OUnit.assert_equal ~printer:Std.dump ~msg:"flags"
    lhs.flags        rhs.flags;
  OUnit.assert_equal ~printer:string_of_insts ~msg:"instructions"
    lhs.instructions rhs.instructions;
  OUnit.assert_equal ~printer:Std.dump ~msg:"traits"
    lhs.traits       rhs.traits;
  OUnit.assert_equal ~printer:Std.dump ~msg:"exceptions"
    lhs.exceptions   rhs.exceptions

let make_meth args inst = {
  name="";
  params=args;
  return=0;
  flags=0;
  instructions=inst;
  traits=[];
  exceptions=[]}


let toplevel inst = 
  make_meth [] ([GetLocal_0;PushScope]@inst@[ReturnValue])

let inner args inst =
  make_meth args (inst@[ReturnValue])

let qname name =
  QName ((Namespace ""),name)

let compile x =
  (generate_method [Expr x])

test lib_call =
    assert_equal 
      (toplevel [FindPropStrict (qname "print");
		 PushString "Hello";
		 CallPropLex ((qname "print"),1)])
      (compile (Call [Var "print";String "Hello"]))

test int = 
  assert_equal 
    (toplevel [PushInt 42])
    (compile (Int 42))

test add = 
  assert_equal
    (toplevel [PushInt 1;PushInt 2;Add_i;])
    (compile (Call [Var "+";Int 1;Int 2]))

test boolean = 
  assert_equal
    (toplevel [PushInt 1;PushInt 2;Equals])
    (compile (Call [Var "=";Int 1;Int 2]))

test block =
  assert_equal
    (toplevel [PushInt 1;Pop;PushInt 2])
    (compile (Block [Int 1;Int 2]))

test if_ =
  let a =
    Label.peek 0 in
  let b = Label.peek 1 in
  assert_equal
    (toplevel [PushInt 10; PushInt 20;  
	       IfNe a; PushInt 0; Jump b;
	       Label a;PushInt 1; Label b])
    (compile (If ((Call [Var "=";Int 10;Int 20]),Int 0,Int 1)))

test let_ =
  assert_equal
    (toplevel [PushString "x"; PushInt 1;
	       PushString "y"; PushInt 2;
	       NewObject 2;
	       PushWith;
	       GetScopeObject 1;
	       GetProperty (qname "x");
	       Pop;
	       GetScopeObject 1;
	       GetProperty (qname "y");
	       PopScope])
    (compile (Let (["x",Int 1;"y",Int 2],
		   Block [Var "x";Var "y"])))

test call =
  assert_equal 
    (toplevel [NewFunction (inner [] [PushInt 42]) ])
    (compile (Lambda ([],Block [Int 42])))

test call_with_args =
  assert_equal 
    (toplevel [NewFunction (inner [0;0] [GetLocal 2])])
    (compile (Lambda (["x";"y"],Block [Var "y"])))

test closure =
    assert_equal 
      (toplevel [NewFunction (inner [] [NewFunction (inner [] [GetLex (qname "x")])]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_method @@ Lisp.compile_string "(define (f) (lambda () x))")

test closure_with_args =
  let name = 
    QName ((Namespace ""),"x") in
    assert_equal 
      (toplevel [NewFunction (inner [0] [PushString "x";GetLocal 1;NewObject 1;PushWith;NewFunction (inner [] [GetLex name]);PopScope]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_method @@ Lisp.compile_string "(define (f x) (lambda () x))")
