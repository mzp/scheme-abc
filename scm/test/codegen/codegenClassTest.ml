open Base
open Asm
open Cpool
open Codegen
open OUnit
open AstUtil
open ISpec

let qname ns x =
  `QName ((`Namespace (String.concat "." ns)),x)

let global name =
  AstUtil.qname [] name

let expr inst =
  {empty_method with
     method_name = qname [] "";
     instructions= [ `GetLocal_0; `PushScope ] @ inst @ [ `Pop; `ReturnVoid ] }

let ok_e expect actual =
  assert_equal ~printer:Std.dump (expect @ [ `Pop ]) @@
    generate_program [`Expr actual]

let ok_s expect actual =
  assert_equal expect @@ generate_program [actual]

let toplevel inst =
  {empty_method with
     method_name  = qname [] "";
     instructions = [ `GetLocal_0; `PushScope ] @ inst @ [ `ReturnVoid ]}

let new_class ({class_name = name; super=super} as c) =
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
  {empty_method with
     method_name  = qname [] "init";
     fun_scope    = `Class (qname [] "Foo");
     instructions = prefix @ [ `ReturnVoid ] }

let cinit =
  {empty_method with
     method_name  = qname [] "cinit";
     fun_scope    = `Class (qname [] "Foo");
     instructions = [ `ReturnVoid ] }

let foo_class =
  {class_name   = qname [] "Foo";
   super            = qname [] "Object";
   class_flags      = [`Sealed];
   cinit            = cinit;
   iinit            = init;
   interface        = [];
   attributes       = [];
   instance_methods = [];
   static_methods   = [];
  }


let class_ name super attrs meths =
  AstUtil.class_ (`Public (global name)) (global super) attrs meths

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
	      class_ "Foo" "Object" [] []);
       "ctor" >::
	 (fun () ->
	    ok_s
	      (new_class
		 {foo_class with
		    iinit     = {init with
				   instructions =
			prefix@[`PushByte 10; `Pop]@[`ReturnVoid] }}) @@
	      class_ "Foo" "Object" [] [
		((public_meth "init" ["self"] (int 10)),[])
	      ]);
       "attributes should be class's attributes" >::
	 (fun () ->
	    ok_s (new_class {foo_class with
			       attributes = [qname [] "x";qname [] "y"] }) @@
	      class_ "Foo" "Object"
	      ["x";"y"] []);
       "method should be class's member" >::
	 (fun ()->
	    ok_s (new_class
		  {foo_class with
		     instance_methods   =
		      [{ empty_method with
			   method_name  = qname [] "f";
			   fun_scope    = `Class (qname [] "Foo");
			   instructions = [`PushByte 42; `ReturnValue] }]}) @@
	      class_ "Foo" "Object" [] [
		public_meth "f" ["self"] (int 42),[]
	      ]);
       "static method should be class's member" >::
	 (fun ()->
	    ok_s (new_class
		  {foo_class with
		     static_methods   =
		      [{ empty_method with
			   method_name = qname [] "f";
			   params = [0];
			   fun_scope = `Class (qname [] "Foo");
			   instructions = [`PushByte 42; `ReturnValue] }]}) @@
	      class_ "Foo" "Object" [] [
		static_meth "f" ["x"] (int 42),[]
	      ]);
       "namespace should be super-class" >::
	 (fun () ->
	    ok_s (new_class
		    {foo_class with
		       super = qname ["flash";"text"] "Object"}) @@
	      AstUtil.class_ (`Public (global "Foo"))
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
	      class_ "Foo" "Object" [] [
		public_meth "init" ["self";"x"] (int 42),[]
	      ]);
       "override" >::
	 (fun () ->
	    ok_s (new_class
		  {foo_class with
		     instance_methods   =
		      [{ empty_method with
			   method_name  = qname [] "f";
			   fun_scope    = `Class (qname [] "Foo");
			   instructions = [`PushByte 42; `ReturnValue];
			   method_attrs = [`Override] }]}) @@
	      class_ "Foo" "Object" [] [
		public_meth "f" ["self"] (int 42),[`Override]
	      ]);
     ]
   ]) +> run_test_tt_main
