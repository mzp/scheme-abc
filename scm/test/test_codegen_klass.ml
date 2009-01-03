open Base
open Asm
open Cpool
open Codegen
open Util
open OUnit
open AstUtil

let expr inst =
  {Asm.empty_method with
     name =
      Cpool.make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[Pop;ReturnVoid]}

let ok_e expect actual =
  assert_equal (expect@[Pop]) @@
    generate_program [`Expr actual]

let ok_s expect actual =
  assert_equal expect @@ generate_program [actual]

let qname name =
  Cpool.make_qname name

let toplevel inst =
  {Asm.empty_method with
     name         = qname "";
     instructions = [GetLocal_0;PushScope]@inst@[ReturnVoid]}

let new_class klass =
  [
    GetLex klass.Asm.sname;
    PushScope;
    GetLex klass.Asm.sname;
    NewClass klass;
    PopScope;
    GetGlobalScope;
    Swap;
    InitProperty klass.Asm.cname
  ]

let prefix= [GetLocal_0;
	     ConstructSuper 0]

let init =
  {Asm.empty_method with
     name =
      qname "init";
     fun_scope =
      Asm.Class (qname "Foo");
     instructions =
      prefix @ [ReturnVoid] }

let cinit =
  {Asm.empty_method with
     name =
      qname "cinit";
     fun_scope =
      Asm.Class (qname "Foo");
     instructions =
      [ReturnVoid] }

let foo_class =
  {Asm.cname = qname "Foo";
   sname     = qname "Object";
   flags_k   = [Asm.Sealed];
   cinit     = cinit;
   iinit     = init;
   interface = [];
   attributes = [];
   methods   = []}

let _ =
  ("codegen.ml(class)" >::: [
     "expr" >::: [
       "new should constructprop" >::
	 (fun () ->
	    ok_e [FindPropStrict (qname "Foo");
		  ConstructProp (qname "Foo",0)] @@
	      `New (global "Foo",[]));
       "new with argument should push it" >::
	 (fun () ->
	    ok_e [FindPropStrict (qname "Foo");
		  PushByte 42;
		  ConstructProp (qname "Foo",1)] @@
	      `New (global "Foo",[int 42]));
       "invoke should use getlex/callproperty" >::
	 (fun () ->
	    ok_e [GetLex (qname "x");
		  PushByte 10;
		  CallProperty (qname "foo",1)] @@
	      `Invoke (var @@ global "x",sname "foo",[int 10]));
       "slot-ref should use GetProperty" >::
	 (fun () ->
	    ok_e [GetLex (qname "obj");
		  GetProperty (qname "x")] @@
	      `SlotRef (var @@ global "obj",sname "x"));
       "slot-set! should use SetProperty" >::
	 (fun () ->
	    ok_e [PushByte 42;
		  GetLex (qname "obj");
		  Swap;
		  SetProperty (qname "x");
		  PushUndefined] @@
	 `SlotSet (var @@ global "obj",sname "x",int 42))
     ];
     "stmt" >::: [
       "klass should use NewClass" >::
	 (fun () ->
	    ok_s (new_class foo_class) @@
	      klass (`Public (global "Foo")) (global "Object") [] []);
       "init method should be constructor" >::
	 (fun () ->
	    ok_s
	      (new_class
		 {foo_class with
		    iinit     = {init with
				   instructions =
			prefix@[PushByte 10;Pop]@[ReturnVoid] }}) @@
	      klass (`Public (global "Foo")) (global "Object") [] [
		public_meth "init" ["self"] (int 10)
	      ]);
       "attributes should be class's attributes" >::
	 (fun () ->
	    ok_s (new_class {foo_class with
			       attributes = [qname "x";qname "y"] }) @@
	      klass (`Public (global "Foo")) (global "Object")
	      ["x";"y"] []);
       "method should be class's member" >::
	 (fun ()->
	    ok_s (new_class
		  {foo_class with
		     methods   =
		      [{ Asm.empty_method with
			   name = qname "f";
			   fun_scope = Asm.Class (qname "Foo");
			   instructions = [PushByte 42;ReturnValue] }]}) @@
	      klass (`Public (global "Foo")) (global "Object") [] [
		public_meth "f" ["self"] (int 42)
	      ]);
       "namespace should be super-class" >::
	 (fun () ->
	    let make ns x =
	      QName ((Namespace ns),x) in
	      ok_s (new_class
		    {foo_class with
		       sname = make "flash.text" "Object"}) @@
		klass (`Public (global "Foo"))
		      (AstUtil.qname "flash.text" "Object") [] []);
       "method arguments should apper params" >::
	 (fun () ->
	    ok_s (new_class
		    {foo_class with
		       iinit = {init with
				  params = [0];
				  instructions = List.concat [
				    prefix;
				    [PushByte 42; Pop;ReturnVoid] ] }}) @@
	      klass (`Public (global "Foo")) (global "Object") [] [
		public_meth "init" ["self";"x"] (int 42)
	      ]);
     ]
   ]) +> run_test_tt
