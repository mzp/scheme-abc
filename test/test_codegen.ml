open Base
open Asm
open Ast
open Cpool
open Codegen

(** util function *)
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
  name=make_qname "";
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

(** test *)
test lib_call =
    assert_equal 
      (toplevel [FindPropStrict (qname "print");
		 PushString "Hello";
		 CallPropLex ((qname "print"),1)])
      (compile (Call [Var "print";String "Hello"]))

(* literal *)
test int = 
  assert_equal 
    (toplevel [PushByte 42])
    (compile (Int 42))

test int_opt =
  assert_equal
    (toplevel [PushByte 42])
    (compile (Int 42));
  assert_equal
    (toplevel [PushInt 300])
    (compile (Int 300))

test string =
  assert_equal
    (toplevel [PushString "Thanks for All the Fish"])
    (compile (String "Thanks for All the Fish"))

(* builtin operator *)
test add = 
  assert_equal
    (toplevel [PushByte 1;PushByte 2;Add_i;])
    (compile (Call [Var "+";Int 1;Int 2]))

test boolean = 
  assert_equal
    (toplevel [PushByte 1;PushByte 2;Equals])
    (compile (Call [Var "=";Int 1;Int 2]))

(* complex expression *)
test block =
  assert_equal
    (toplevel [PushByte 1;Pop;PushByte 2])
    (compile (Block [Int 1;Int 2]))

test if_ =
  let a =
    Label.peek 0 in
  let b = Label.peek 1 in
  assert_equal
    (toplevel [PushByte 10; PushByte 20;  
	       IfNe a; PushByte 0; Jump b;
	       Label a;PushByte 1; Label b])
    (compile (If ((Call [Var "=";Int 10;Int 20]),Int 0,Int 1)))

(* scope *)
test let_ =
  assert_equal
    (toplevel [PushString "x"; PushByte 1;
	       PushString "y"; PushByte 2;
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

test letrec =
    assert_equal
      (toplevel [NewObject 0;
		 PushWith;
		 GetScopeObject 1;
		 PushByte 42;
		 SetProperty (qname "x");
		 PopScope])
      (compile (LetRec (["x",Int 42],Block [])))

test define =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_method @@ Lisp.compile_string "(define (f) 42)")

test define_not_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 0;Swap;SetProperty (qname "g")])
      (generate_method @@ Lisp.compile_string "(define (f) 42) (define (g) 30)")

test define_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewObject 0;PushWith;
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 1;Swap;SetProperty (qname "f")])
      (generate_method @@ Lisp.compile_string "(define (f) 42) (define (f) 30)")

test closure =
    assert_equal 
      (toplevel [NewFunction (inner [] [NewFunction (inner [] [GetLex (qname "x")])]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_method @@ Lisp.compile_string "(define (f) (lambda () x))")

(* function call *)
test call =
  assert_equal 
    (toplevel [NewFunction (inner [] [PushByte 42]) ])
    (compile (Lambda ([],Block [Int 42])))

test call_with_args =
  assert_equal 
    (toplevel [NewFunction (inner [0;0] [GetLocal 2])])
    (compile (Lambda (["x";"y"],Block [Var "y"])))
