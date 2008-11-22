open Base
open Asm
open Ast2
open Cpool
open Codegen
open Util
open OUnit

let node x =
  {(Node.empty x) with Node.filename = "<string>"; Node.lineno = 0}

  
let string x =
  String (node x)

let int x =
  Int (node x)

let float x =
  Float (node x)

let bool x =
  Bool (node x)

let var x =
  Var (node x)


(** util function *)
let compile_string str =
  ClosTrans2.trans @@ Lisp2.compile_string str

let string_of_insts xs =
  let ys =
    String.concat "; \n\t" @@ List.map string_of_instruction xs in
    Printf.sprintf "[\n\t%s ]\n" ys

let ok lhs rhs =
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

let _ =
  ("codegen module test" >::: [
     "external call" >::
       (fun () ->
	  ok (expr [FindPropStrict (qname "print");
		    PushString "Hello";
		    CallPropLex ((qname "print"),1)]) @@
	    compile (Call [var "print";string "Hello"]));
     "literal" >::: [
       "int" >::
	 (fun () ->
	    ok (expr [PushByte 42]) @@
	      compile (int 42));
       "int optimize" >::
	 (fun () ->
	    ok (expr [PushByte 42]) @@
	      compile (int 42);
	    ok (expr [PushInt 300]) @@
	      compile (int 300));
       "string" >::
	 (fun () ->
	    ok (expr [PushString "Thanks for All the Fish"]) @@
	      compile (string "Thanks for All the Fish"))
     ];
     "bulidin operator" >::: [
       "+" >::
	 (fun () ->
	    ok (expr [PushByte 1;PushByte 2;Add_i]) @@
	      compile (Call [var "+";int 1;int 2]));
       "=" >::
	 (fun () ->
	    ok (expr [PushByte 1;PushByte 2;Equals]) @@
	      compile (Call [var "=";int 1;int 2]))
     ];
     "if" >::
       (fun () ->
	  let a =
	    Label.peek 0 in
	  let b = 
	    Label.peek 1 in
	    ok 
	      (expr [PushByte 10; PushByte 20;  
		     IfNe a; PushByte 0; Jump b;
		     Asm.Label a;PushByte 1; Asm.Label b])
	      (compile (If ((Call [var "=";int 10;int 20]),int 0,int 1))));
     "block" >::
       (fun () ->
	  ok (expr [PushByte 1;Pop;PushByte 2]) @@
	    compile (Block [int 1;int 2]));
     "let" >::
       (fun () ->
	  ok (expr [PushString "x"; PushByte 1;
		    PushString "y"; PushByte 2;
		    NewObject 2;
		    PushWith;
		    GetScopeObject 1;
		    GetProperty (qname "x");
		    Pop;
		    GetScopeObject 1;
		    GetProperty (qname "y");
		    PopScope]) @@
	    compile (Let ([node "x",int 1;node "y",int 2],
			  Block [var "x";var "y"])));
     "letrec" >::
       (fun () ->
	  ok (expr [NewObject 0;
		    PushWith;
		    GetScopeObject 1;
		    PushByte 42;
		    SetProperty (qname "x");
		    PushByte 10;
		    PopScope]) @@
	    compile (LetRec ([node "x",int 42],Block [int 10])));
     "letrec for recursion" >::
       (fun () ->
	  ok (expr [NewObject 0;
		    PushWith;
		    GetScopeObject 1;

		    GetScopeObject 1;
		    GetProperty (qname "x");
		    
		    SetProperty (qname "x");

		    PushByte 42;
		    PopScope]) @@
	    compile (LetRec ([node "x",var "x"],Block [int 42])));
     "define" >::
       (fun () ->
	  ok (toplevel [NewFunction (inner [] [PushByte 42]);
			GetScopeObject 0;
			Swap;
			SetProperty (qname "f")]) @@
	    generate_script @@ compile_string "(define (f) 42)");
     "define dual" >::
       (fun () ->
	  ok (toplevel [NewFunction (inner [] [PushByte 42]);
			GetScopeObject 0;Swap;SetProperty (qname "f");
			NewFunction (inner [] [PushByte 30]);
			GetScopeObject 0;Swap;SetProperty (qname "g")]) @@
	    generate_script @@ compile_string 
	    "(define (f) 42) (define (g) 30)");
     "define same name" >::
       (fun () ->
	  ok (toplevel [NewFunction (inner [] [PushByte 42]);
			GetScopeObject 0;Swap;SetProperty (qname "f");
			NewObject 0;PushWith;
			NewFunction (inner [] [PushByte 30]);
			GetScopeObject 1;Swap;SetProperty (qname "f")]) @@
	  generate_script @@ compile_string "(define (f) 42) (define (f) 30)");
     "closure" >::
       (fun () ->
	  ok (toplevel [NewFunction 
			  (inner [] [NewFunction
				       (inner [] [GetLex (qname "x")])]);
			GetScopeObject 0;
			Swap;
			SetProperty (qname "f")]) @@
	    generate_script @@ compile_string "(define (f) (lambda () x))");
     "call" >::: [
       "normal" >::
	 (fun () ->
	    ok  (expr [NewFunction (inner [] [PushByte 42]) ]) @@
	      compile (Lambda ([],Block [int 42])));
       "arguments" >::
	 (fun () ->
	    ok  (expr [NewFunction (inner [0;0] [GetLocal 2])]) @@
	      compile (Lambda ([node "x";node "y"],Block [var "y"])));
       "lambda" >::
	 (fun () ->
	    ok  (expr [PushString "z"; PushByte 42;
		       NewObject 1;
		       PushWith;
		       NewFunction (inner [] [GetLex (qname "z")]);
		       PopScope]) @@
	      compile (Let ([node "z",int 42],
			    Lambda ([],Block [var "z"]))))
     ];
     "class" >::: [
       "new" >::
	 (fun () ->
	    ok (expr [FindPropStrict (make_qname "Foo");
		      ConstructProp (make_qname "Foo",0)]) @@
	      generate_script @@ compile_string "(new Foo)");
       "new arguments" >::
	 (fun () ->
	    ok (expr [FindPropStrict (make_qname "Foo");
		      PushByte 42;ConstructProp (make_qname "Foo",1)]) @@
	      generate_script @@ compile_string "(new Foo 42)");
       "invoke" >::
	 (fun () ->
	    ok (expr [GetLex (make_qname "x");
		      PushByte 10;
		      CallProperty (make_qname "foo",1)]) @@
	      generate_script @@ compile_string "(. x (foo 10))");
       "slot-ref" >::
	 (fun () ->
	    ok (expr [GetLex (make_qname "obj");
		      GetProperty (make_qname "x")]) @@
	      generate_script @@ compile_string "(slot-ref obj x)");
       "slot-set!" >::
	 (fun () ->
	    ok (expr [PushByte 42; 
		      GetLex (make_qname "obj");
		      Swap;
		      SetProperty (make_qname "x");
		      PushUndefined]) @@
	      generate_script @@ compile_string "(slot-set! obj x 42)");
     ];
     "class define" >::: [
       "normal" >::
	 (fun () ->
	    ok 
	      (new_class
		 {Asm.cname = make_qname "Foo"; 
		  sname     = make_qname "Object";
		  flags_k   = [Asm.Sealed];
		  attributes = [];
		  cinit     = cinit;
		  iinit     = {init with
				 instructions = 
		      prefix@[PushByte 10;Pop]@[ReturnVoid] };
		  interface = [];
		  methods   = []}) @@
	      generate_script @@ compile_string "(define-class Foo (Object) ())
          (define-method init ((self Foo)) 10)");
       "empty" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo"; 
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes= [];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   methods   = []}) @@
	      generate_script @@ compile_string 
	      "(define-class Foo (Object) ())");
       "method" >::
	 (fun ()->
	    ok (new_class
		  {Asm.cname = make_qname "Foo"; 
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes= [];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   methods   = [{ Asm.empty_method with
				    name = make_qname "f";
				    fun_scope = Asm.Class (make_qname "Foo");
				    instructions = [PushByte 42;ReturnValue] }]}) @@
	      generate_script @@ compile_string 
		 "(define-class Foo (Object) ())
          (define-method f ((self Foo)) 42)");
       "namespace" >::
	 (fun () ->
	    let make ns x =
	      QName ((Namespace ns),x) in
	      ok (new_class 
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
		     methods   = []}) @@
		generate_script @@ compile_string 
		"(define-class Foo (flash.text.Object) ())
              (define-method init ((self Foo))  42)");
       "method arguments" >::
	 (fun () ->
	    ok (new_class
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
		   methods   = []}) @@
	      generate_script @@ compile_string 
	      "(define-class Foo (Object) ())
          (define-method init ((self Foo) x) x)");
       "self" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo"; 
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   attributes = [];
		   cinit     = cinit;
		   iinit     = {init with
				  instructions = prefix @ [GetLocal 0;Pop;ReturnVoid] };
		   interface = [];
		   methods   = []}) @@
	      generate_script @@ compile_string 
	      "(define-class Foo (Object) ())
          (define-method init ((self Foo)) self)");
       "attributes" >::
	 (fun () ->
	    ok (new_class
		  {Asm.cname = make_qname "Foo"; 
		   sname     = make_qname "Object";
		   flags_k   = [Asm.Sealed];
		   cinit     = cinit;
		   iinit     = init;
		   interface = [];
		   attributes = [Cpool.make_qname "x";Cpool.make_qname "y"];
		   methods   = []}) @@
	      generate_script @@ compile_string "(define-class Foo (Object) (x y))");
     ]
   ]) +> run_test_tt


