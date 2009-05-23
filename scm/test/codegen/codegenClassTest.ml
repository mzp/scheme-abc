open Base
open Asm
open Cpool
open Codegen
open OUnit
open AstUtil

let qname ns x =
  `QName ((`Namespace (String.concat "." ns)),x)

let global name =
  AstUtil.qname [] name

let expr inst =
  {Asm.empty_method with
     method_name = qname [] "";
     instructions= [ `GetLocal_0; `PushScope ] @ inst @ [ `Pop; `ReturnVoid ] }

let ok_e expect actual =
  assert_equal ~printer:Std.dump (expect @ [ `Pop ]) @@
    generate_program [`Expr actual]

let ok_s expect actual =
  assert_equal expect @@ generate_program [actual]

let toplevel inst =
  {Asm.empty_method with
     method_name  = qname [] "";
     instructions = [ `GetLocal_0; `PushScope ] @ inst @ [ `ReturnVoid ]}

let new_class ({Asm.class_name = name; super=super} as c) =
  [
    `GetLex super;
    `PushScope;
    `GetLex super;
    `NewClass c;
    `PopScope;
    `GetGlobalScope;
    `Swap;
    `InitProperty name;
  ]

let prefix= [
  `GetLocal_0;
  `ConstructSuper 0]

let init =
  {Asm.empty_method with
     method_name  = qname [] "init";
     fun_scope    = Asm.Class (qname [] "Foo");
     instructions = prefix @ [ `ReturnVoid ] }

let cinit =
  {Asm.empty_method with
     method_name  = qname [] "cinit";
     fun_scope    = Asm.Class (qname [] "Foo");
     instructions = [ `ReturnVoid ] }

let foo_class =
  {Asm.class_name   = qname [] "Foo";
   super            = qname [] "Object";
   class_flags      = [Asm.Sealed];
   cinit            = cinit;
   iinit            = init;
   interface        = [];
   attributes       = [];
   instance_methods = [];
   static_methods   = [];
  }

let _ =
  ("codegen.ml(class)" >::: [
     "expr" >::: [
       "new" >::
	 (fun () ->
	    ok_e [`FindPropStrict (qname [] "Foo");
		  `ConstructProp (qname [] "Foo",0)] @@
	      new_ [] "Foo" []);
       "new(args)" >::
	 (fun () ->
	    ok_e [`FindPropStrict (qname [] "Foo");
		  `PushByte 42;
		  `ConstructProp (qname [] "Foo",1)] @@
	      new_ [] "Foo" [int 42]);
       "invoke" >::
	 (fun () ->
	    ok_e [`GetLex (qname [] "x");
		  `PushByte 10;
		  `CallProperty (qname [] "foo",1)] @@
	      invoke (var [] "x") "foo" [int 10]);
       "slot-ref" >::
	 (fun () ->
	    ok_e [`GetLex (qname [] "obj");
		  `GetProperty (qname [] "x")] @@
	      slot_ref (var [] "obj") "x");
       "slot-set!" >::
	 (fun () ->
	    ok_e [`PushByte 42;
		  `GetLex (qname [] "obj");
		  `Swap;
		  `SetProperty (qname [] "x");
		  `PushUndefined] @@
	      slot_set (var [] "obj") "x" (int 42))
     ];
     "stmt" >::: [
       "class" >::
	 (fun () ->
	    ok_s (new_class foo_class) @@
	      class_ (`Public (global "Foo")) (global "Object") [] []);
       "ctor" >::
	 (fun () ->
	    ok_s
	      (new_class
		 {foo_class with
		    iinit     = {init with
				   instructions =
			prefix@[`PushByte 10; `Pop]@[`ReturnVoid] }}) @@
	      class_ (`Public (global "Foo")) (global "Object") [] [
		public_meth "init" ["self"] (int 10)
	      ]);
       "attributes should be class's attributes" >::
	 (fun () ->
	    ok_s (new_class {foo_class with
			       attributes = [qname [] "x";qname [] "y"] }) @@
	      class_ (`Public (global "Foo")) (global "Object")
	      ["x";"y"] []);
       "method should be class's member" >::
	 (fun ()->
	    ok_s (new_class
		  {foo_class with
		     instance_methods   =
		      [{ Asm.empty_method with
			   method_name  = qname [] "f";
			   fun_scope    = Asm.Class (qname [] "Foo");
			   instructions = [`PushByte 42; `ReturnValue] }]}) @@
	      class_ (`Public (global "Foo")) (global "Object") [] [
		public_meth "f" ["self"] (int 42)
	      ]);
       "static method should be class's member" >::
	 (fun ()->
	    ok_s (new_class
		  {foo_class with
		     static_methods   =
		      [{ Asm.empty_method with
			   method_name = qname [] "f";
			   params = [0];
			   fun_scope = Asm.Class (qname [] "Foo");
			   instructions = [`PushByte 42; `ReturnValue] }]}) @@
	      class_ (`Public (global "Foo")) (global "Object") [] [
		static_meth "f" ["x"] (int 42)
	      ]);
       "namespace should be super-class" >::
	 (fun () ->
	    ok_s (new_class
		    {foo_class with
		       super = qname ["flash";"text"] "Object"}) @@
	      class_ (`Public (global "Foo"))
	      (AstUtil.qname ["flash";"text"] "Object") [] []);
       "method arguments should apper params" >::
	 (fun () ->
	    ok_s (new_class
		    {foo_class with
		       iinit = {init with
				  params = [0];
				  instructions = List.concat [
				    prefix;
				    [`PushByte 42; `Pop; `ReturnVoid] ] }}) @@
	      class_ (`Public (global "Foo")) (global "Object") [] [
		public_meth "init" ["self";"x"] (int 42)
	      ]);
     ]
   ]) +> run_test_tt
