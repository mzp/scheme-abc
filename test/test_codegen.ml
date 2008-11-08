open Base
open Asm
open Ast
open Cpool
open Codegen
open Util

(** util function *)
let string_of_insts xs =
  let ys =
    String.concat "; \n\t" @@ List.map string_of_instruction xs in
    Printf.sprintf "[\n\t%s ]\n" ys

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

let expr inst = 
  {Asm.empty_method with
     name =
      make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[Pop;ReturnVoid]}

let toplevel inst = 
  {Asm.empty_method with
     name =
      make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[ReturnVoid]}

let inner args inst =
  {Asm.empty_method with
     name =
      make_qname "";
     params =
      args;
     instructions=
      inst@[ReturnValue] }

let qname name =
  QName ((Namespace ""),name)

let compile x =
  (generate_script [Expr x])

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
		 PushByte 10;
		 PopScope])
      (compile (LetRec (["x",Int 42],Block [Int 10])))

test letrec =
    assert_equal
      (expr [NewObject 0;
	     PushWith;
	     GetScopeObject 1;

	     GetScopeObject 1;
	     GetProperty (qname "x");

	     SetProperty (qname "x");

	     PushByte 42;
	     PopScope])
      (compile (LetRec (["x",Var "x"],Block [Int 42])))

test define =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_script @@ compile_string "(define (f) 42)")

test define_not_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 0;Swap;SetProperty (qname "g")])
      (generate_script @@ compile_string "(define (f) 42) (define (g) 30)")

test define_hidden =
    assert_equal 
      (toplevel [NewFunction (inner [] [PushByte 42]);GetScopeObject 0;Swap;SetProperty (qname "f");
		 NewObject 0;PushWith;
		 NewFunction (inner [] [PushByte 30]);GetScopeObject 1;Swap;SetProperty (qname "f")])
      (generate_script @@ compile_string "(define (f) 42) (define (f) 30)")

test closure =
    assert_equal 
      (toplevel [NewFunction (inner [] [NewFunction (inner [] [GetLex (qname "x")])]);
		 GetScopeObject 0;
		 Swap;
		 SetProperty (qname "f")])
      (generate_script @@ compile_string "(define (f) (lambda () x))")

(* function call *)
test call =
  assert_equal 
    (expr [NewFunction (inner [] [PushByte 42]) ])
    (compile (Lambda ([],Block [Int 42])))

test call_with_args =
  assert_equal 
    (expr [NewFunction (inner [0;0] [GetLocal 2])])
    (compile (Lambda (["x";"y"],Block [Var "y"])))

test closure_lambda =
  assert_equal 
    (expr [PushString "z"; PushByte 42;
	   NewObject 1;
	   PushWith;
	   NewFunction (inner [] [GetLex (qname "z")]);
	   PopScope])
    (compile (Let (["z",Int 42],
		   Lambda ([],Block [Var "z"]))))


test new_ = 
  assert_equal 
    (expr [FindPropStrict (make_qname "Foo");ConstructProp (make_qname "Foo",0)])
    (generate_script @@ compile_string "(new Foo)")

test new_ = 
  assert_equal 
    (expr [FindPropStrict (make_qname "Foo");PushByte 42;ConstructProp (make_qname "Foo",1)])
    (generate_script @@ compile_string "(new Foo 42)")

test invoke =
  assert_equal
    (expr [GetLex (make_qname "x");PushByte 10;CallProperty (make_qname "foo",1)])
    (generate_script @@ compile_string "(. x (foo 10))")

test slotref =
  assert_equal
    (expr [GetLex (make_qname "obj");GetProperty (make_qname "x")])
    (generate_script @@ compile_string "(slot-ref obj x)")

test slotsef =
  assert_equal
    (expr [PushByte 42; 
	   GetLex (make_qname "obj");
	   Swap;
	   SetProperty (make_qname "x");
	   PushUndefined])
    (generate_script @@ compile_string "(slot-set! obj x 42)")


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

let init =
  {Asm.empty_method with
     name = 
      make_qname "init";
     fun_scope =
      Asm.Class (make_qname "Foo");
     instructions =
      prefix @ [ReturnVoid] }

let cinit =
  {Asm.empty_method with
     name = 
      make_qname "cinit";
     fun_scope =
      Asm.Class (make_qname "Foo");
     instructions =
      [ReturnVoid] }
    
test klass =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  attributes = [];
	  cinit     = cinit;
	  iinit     = {init with
			 instructions = prefix@[PushByte 10;Pop]@[ReturnVoid] };
	  interface = [];
	  methods   = []})
      (generate_script @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo)) 10)")

test klass_empty =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  attributes = [];
	  cinit     = cinit;
	  iinit     = init;
	  interface = [];
	  methods   = []})
      (generate_script @@ compile_string "(define-class Foo (Object) ())")

test klass_f =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  attributes = [];
	  cinit     = cinit;
	  iinit     = init;
	  interface = [];
	  methods   = [{ Asm.empty_method with
			   name = make_qname "f";
			   fun_scope = Asm.Class (make_qname "Foo");
			   instructions = [PushByte 42;ReturnValue] }]})
      (generate_script @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method f ((self Foo)) 42)")

test klass_with_ns =
      let make ns x =
	QName ((Namespace ns),x) in
	assert_equal 
	  (new_class 
	     {Asm.cname = 
		 make_qname "Foo"; 
	      sname =
		 make "flash.text" "Object";
	      flags_k =
		 [Asm.Sealed];
	      attributes = 
		 [];
	      cinit = 
		 cinit;
	      iinit = 
		 {init with 
		    instructions = prefix @ [PushByte 42; Pop; ReturnVoid]};
	      interface = [];
	      methods   = []})
	  (generate_script @@ compile_string 
	     "(define-class Foo (flash.text.Object) ())
              (define-method init ((self Foo))  42)")

test klass_args =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  attributes = [];
	  cinit     = cinit;
	  iinit     = {init with
			 params = [0];
			 instructions = List.concat [
			   prefix;
			   [GetLocal 1; Pop;ReturnVoid] ] };
	  interface = [];
	  methods   = []})
      (generate_script @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo) x) x)")

test klass_self =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  attributes = [];
	  cinit     = cinit;
	  iinit     = {init with
			 instructions = prefix @ [GetLocal 0;Pop;ReturnVoid] };
	  interface = [];
	  methods   = []})
      (generate_script @@ compile_string 
	 "(define-class Foo (Object) ())
          (define-method init ((self Foo)) self)")


test klass_f_args =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = cinit;
	  iinit     = init;
	  interface = [];
	  attributes = [];
	  methods   = [{Asm.empty_method with
			  name = 
			   make_qname "f";
			  fun_scope = 
			   Asm.Class (make_qname "Foo");
			  params = 
			   [0];
			  instructions =
			   [GetLocal 1;ReturnValue] }]})
      (generate_script @@ compile_string "(define-class Foo (Object) ())
 (define-method f ((self Foo) x) x)")

test klass_attr =
    assert_equal 
      (new_class
	 {Asm.cname = make_qname "Foo"; 
	  sname     = make_qname "Object";
	  flags_k   = [Asm.Sealed];
	  cinit     = cinit;
	  iinit     = init;
	  interface = [];
	  attributes = [Cpool.make_qname "x";Cpool.make_qname "y"];
	  methods   = []})
      (generate_script @@ compile_string "(define-class Foo (Object) (x y))")
