open Base
open Asm
open Ast
open Cpool
open Codegen
open Util

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

let expr inst = 
  make_meth [] ([GetLocal_0;PushScope]@inst@[Pop;ReturnVoid])

let toplevel inst = 
  make_meth [] ([GetLocal_0;PushScope]@inst@[ReturnVoid])

let inner args inst =
  make_meth args (inst@[ReturnValue])

let qname name =
  QName ((Namespace ""),name)

let compile x =
  (generate_method [Expr x])

(** test *)
test lib_call =
    assert_equal 
      (expr [FindPropStrict (qname "print");
		 PushString "Hello";
		 CallPropLex ((qname "print"),1)])
      (compile (Call [Var "print";String "Hello"]))

(* literal *)
test int = 
  assert_equal 
    (expr [PushByte 42])
    (compile (Int 42))

test int_opt =
  assert_equal
    (expr [PushByte 42])
    (compile (Int 42));
  assert_equal
    (expr [PushInt 300])
    (compile (Int 300))

test string =
  assert_equal
    (expr [PushString "Thanks for All the Fish"])
    (compile (String "Thanks for All the Fish"))

(* builtin operator *)
test add = 
  assert_equal
    (expr [PushByte 1;PushByte 2;Add_i;])
    (compile (Call [Var "+";Int 1;Int 2]))

test boolean = 
  assert_equal
    (expr [PushByte 1;PushByte 2;Equals])
    (compile (Call [Var "=";Int 1;Int 2]))

(* complex expression *)
test block =
  assert_equal
    (expr [PushByte 1;Pop;PushByte 2])
    (compile (Block [Int 1;Int 2]))

test if_ =
  let a =
    Label.peek 0 in
  let b = Label.peek 1 in
  assert_equal
    (expr [PushByte 10; PushByte 20;  
	       IfNe a; PushByte 0; Jump b;
	       Label a;PushByte 1; Label b])
    (compile (If ((Call [Var "=";Int 10;Int 20]),Int 0,Int 1)))

(* scope *)
test let_ =
  assert_equal
    (expr [PushString "x"; PushByte 1;
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
      (expr [NewObject 0;
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
      (generate_method @@ compile_string "(define (f) 42)")

test define_not_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 0;Swap;SetProperty (qname "g")])
      (generate_method @@ compile_string "(define (f) 42) (define (g) 30)")

test define_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewObject 0;PushWith;
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 1;Swap;SetProperty (qname "f")])
      (generate_method @@ compile_string "(define (f) 42) (define (f) 30)")

test closure =
    assert_equal 
      (toplevel [NewFunction (inner [] [NewFunction (inner [] [GetLex (qname "x")])]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_method @@ compile_string "(define (f) (lambda () x))")

(* function call *)
test call =
  assert_equal 
    (expr [NewFunction (inner [] [PushByte 42]) ])
    (compile (Lambda ([],Block [Int 42])))

test call_with_args =
  assert_equal 
    (expr [NewFunction (inner [0;0] [GetLocal 2])])
    (compile (Lambda (["x";"y"],Block [Var "y"])))

test new_ = 
  assert_equal 
    (expr [FindPropStrict (make_qname "Foo");ConstructProp (make_qname "Foo",0)])
    (generate_method @@ compile_string "(new Foo)")

test new_ = 
  assert_equal 
    (expr [FindPropStrict (make_qname "Foo");PushByte 42;ConstructProp (make_qname "Foo",1)])
    (generate_method @@ compile_string "(new Foo 42)")

test invoke =
  assert_equal
    (expr [GetLex (make_qname "x");PushByte 10;CallProperty (make_qname "foo",1)])
    (generate_method @@ compile_string "(. x (foo 10))")


let new_class klass = 
  (toplevel [
     GetLex klass.Asm.sname;
     PushScope;
     GetLex klass.Asm.sname;
     NewClass klass;
     PopScope;
     GetGlobalScope;
     Swap;
     InitProperty klass.Asm.cname])

let prefix= [GetLocal_0;
	     ConstructSuper 0]

test klass =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init"  @@ prefix@[PushByte 10];
	  interface = [];
	  methods   = []})
      (generate_method @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo)) 10)")

test klass_empty =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init" prefix;
	  interface = [];
	  methods   = []})
      (generate_method @@ compile_string "(define-class Foo (Object) ())")

test klass_f =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init" prefix;
	  interface = [];
	  methods   = [Asm.make_meth "f" [PushByte 42]]})
      (generate_method @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method f ((self Foo)) 42)")

test klass_with_ns =
      let make ns x =
	QName ((Namespace ns),x) in
	assert_equal 
	  (new_class {Asm.cname = make_qname "Foo"; 
		      sname     = make "flash.text" "Object";
		      flags_k   = [Asm.Sealed];
		      cinit     = Asm.make_proc "cinit" [];
		      iinit     = Asm.make_proc "init" @@ prefix@[PushByte 10];
		      interface = [];
		      methods   = []})
	  (generate_method @@ compile_string 
	     "(define-class Foo (flash.text.Object) ())
              (define-method init ((self Foo))  10)")

test klass_args =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init" ~args:[0] @@ prefix@[GetLocal 1];
	  interface = [];
	  methods   = []})
      (generate_method @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo) x) x)")

test klass_self =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init" ~args:[] @@ prefix@[GetLocal 0];
	  interface = [];
	  methods   = []})
      (generate_method @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo)) self)")


test klass_f_args =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = Asm.make_proc "cinit" [];
	  iinit     = Asm.make_proc "init" prefix;
	  interface = [];
	  methods   = [Asm.make_meth "f" ~args:[0] [GetLocal 1]]})
      (generate_method @@ compile_string "(define-class Foo (Object) ())
 (define-method f ((self Foo) x) x)")
