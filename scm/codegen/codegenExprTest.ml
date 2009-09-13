open Base
open Codegen
open OUnit
open Binding
open AstUtil
open Swflib
open Swflib.MethodType

let join xs =
  String.concat "." xs

let qname ns x =
  `QName ((`Namespace (join ns)),x)

let ok expect actual =
  assert_equal (expect@[ `Pop ]) @@
    generate_program [`Expr actual]

let member i ns name =
  `BindVar (Node.ghost (Member ((Scope i),(ns,name))))

let global_member ns name =
  `BindVar (Node.ghost (Member (Global,(ns,name))))

let slot i j =
  `BindVar (Node.ghost (Slot ((Scope i),j)))

let register i =
  `BindVar (Node.ghost (Register i))

let inner args inst =
  let l = Label.peek 0 in
    {empty with
       method_name  = qname [] @@ Label.to_string l;
       params       = args;
       code = inst@[`ReturnValue] }

let _ =
  ("codegen.ml(expr)" >::: [
     "literal" >::: [
       "int" >::
	 (fun () ->
	    ok [`PushByte 42] (int 42));
       "int(big)" >::
	 (fun () ->
	    ok [`PushInt 200] (int 200);
	    ok [`PushInt 300] (int 300));
       "string" >::
	 (fun () ->
	    ok [`PushString "Thanks for All the Fish"] @@
	      string "Thanks for All the Fish");
       "float" >::
	 (fun () ->
	    ok [`PushDouble 4.2] @@
	      float 4.2);
       "bool" >::
	 (fun () ->
	    ok [`PushTrue] @@
	      bool true;
	    ok [`PushFalse] @@
	      bool false);
       "array" >::
	 (fun () ->
	    ok [`NewArray 0] (`Array []);
	    ok [`PushByte 1; `PushByte 2; `NewArray 2] (`Array [int 1; int 2]););
     ];
     "if" >::
       (fun () ->
	  let a =
	    Label.peek 0 in
	  let b =
	    Label.peek 1 in
	    ok
	      [`PushByte 10;
	       `PushByte 20;
	       `IfNe a;  `PushByte 0; `Coerce_a; `Jump b;
	       `Label a; `PushByte 1; `Coerce_a; `Label b] @@
	      if_ (call [var [] "="; int 10; int 20]) (int 0) (int 1));
     "if" >::
       (fun () ->
	  List.iter
	    (fun (sym,op) ->
	       let a =
		 Label.peek 0 in
	       let b =
		 Label.peek 1 in
		 ok
		   [`PushByte 10;
		    `PushByte 20;
		    op a;  `PushByte 0; `Coerce_a; `Jump b;
		    `Label a; `PushByte 1; `Coerce_a; `Label b] @@
		   if_ (call [var [] sym; int 10; int 20]) (int 0) (int 1))
	    [("=" ,fun x -> `IfNe x);
	     ("<=",fun x -> `IfNle x);
	     (">" ,fun x -> `IfNgt x);
	     (">=",fun x -> `IfNge x); ]);
     "block" >::
       (fun () ->
	  ok [`PushByte 1; `Pop; `PushByte 2] @@
	    block [int 1;int 2]);
     "empty block" >::
       (fun () ->
	  ok [`PushUndefined] @@
	    block []);
     "let" >::
       (fun () ->
	  ok [`PushString "x"; `PushByte 1;
	      `PushString "y"; `PushByte 2;
	      `NewObject 2;
	      `PushWith;
	      `PushUndefined;
	      `PopScope] @@
	    let_ ["x",int 1; "y",int 2] @@ block []);
     "letrec" >::
       (fun () ->
	  ok [`NewObject 0;
	      `Dup;
	      `PushWith;
	      `Dup;
	      `PushByte 42;
	      `SetProperty (qname [] "x");
	      `Pop;
	      `PushUndefined;
	      `PopScope] @@
	    let_rec ["x",int 42] @@ block []);
     "lambda" >::
       (fun () ->
	  let m =
	    inner [] [`GetLex (qname [] "z")] in
	    ok  [`NewFunction m] @@
	      lambda [] @@ block [var [] "z"]);
     "var" >::: [
       "member" >::
	 (fun () ->
	    ok [`GetScopeObject 1;
		`GetProperty (qname [] "foo")] @@
	      member 1 [] "foo";
	    ok [`GetScopeObject 1;
		`GetProperty (qname [] "bar")] @@
	      member 1 [] "bar");
       "global" >::
	 (fun () ->
	    ok [`GetGlobalScope;
		`GetProperty (qname [] "foo")] @@
	      global_member [] "foo");
       "slot" >::
	 (fun () ->
	    ok [`GetScopeObject 0;
		`GetSlot 1] @@
	      slot 0 1);
       "register" >::
	 (fun () ->
	    ok [`GetLocal 0] @@
	      register 0;
	    ok [`GetLocal 1] @@
	      register 1)
     ];
     "call" >::: [
       "var" >::
	 (fun () ->
	    ok [`FindPropStrict (qname [] "print");
		`PushString "Hello";
		`CallPropLex ((qname [] "print"),1)] @@
	      call [var [] "print"; string "Hello"]);
       "member" >::
	 (fun () ->
	    ok  [`GetScopeObject 1;
		 `PushByte 42;
		 `CallPropLex ((qname [] "f"),1)] @@
	      call [member 1 [] "f"; int 42]);
       "global member" >::
	 (fun () ->
	    ok  [`GetGlobalScope;
		 `PushByte 42;
		 `CallPropLex ((qname [] "f"),1)] @@
	      call [global_member [] "f"; int 42]);
       "bulid-in" >::
	 (fun () ->
	    ok [`PushByte 1; `PushByte 2; `Add_i] @@
	      call [var [] "+";int 1;int 2];
	    ok [`PushByte 1; `PushByte 2; `Equals] @@
	      call [var [] "=";int 1;int 2])
     ];
   ]) +> run_test_tt_main
