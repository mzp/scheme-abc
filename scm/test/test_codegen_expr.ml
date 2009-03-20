open Base
open Asm
open Codegen
open VarResolve
open OUnit
open AstUtil

let qname =
  Cpool.make_qname

let ok expect actual =
  assert_equal (expect@[Pop]) @@
    generate_program [`Expr actual]

let member i ns name =
  `BindVar (node (Member ((Scope i),(ns,name))))

let global_member ns name =
  `BindVar (node (Member (Global,(ns,name))))

let slot i j =
  `BindVar (node (Slot ((Scope i),j)))

let register i =
  `BindVar (node (Register i))

let inner args inst =
  let l = Label.peek 0 in
    {Asm.empty_method with
       name         = qname @@ Label.to_string l;
       params       = args;
       instructions = inst@[ReturnValue] }

let _ =
  ("codegen.ml(expr)" >::: [
     "literal" >::: [
       "small int should be PushByte" >::
	 (fun () ->
	    ok [PushByte 42] @@
	      int 42);
       "big int should be PushInt" >::
	 (fun () ->
	    ok [PushInt 300] @@
	      int 300);
       "string should be PushString" >::
	 (fun () ->
	    ok [PushString "Thanks for All the Fish"] @@
	      string "Thanks for All the Fish");
       "float should be PushDobule" >::
	 (fun () ->
	    ok [PushDouble 4.2] @@
	      float 4.2);
       "bool should be PushTrue/PushFalse" >::
	 (fun () ->
	    ok [PushTrue] @@
	      bool true;
	    ok [PushFalse] @@
	      bool false)
     ];
     "if should be IfNe" >::
       (fun () ->
	  let a =
	    Label.peek 0 in
	  let b =
	    Label.peek 1 in
	    ok
	      [PushByte 10; PushByte 20;
	       IfNe a; PushByte 0; Coerce_a; Jump b;
	       Asm.Label a;PushByte 1; Coerce_a; Asm.Label b] @@
	      `If ((`Call [var @@ global "=";int 10;int 20]),
		   int 0,int 1));
     "block should insert pop between instruction" >::
       (fun () ->
	  ok [PushByte 1;Pop;PushByte 2] @@
	    `Block [int 1;int 2]);
     "empty block should push undefine" >::
       (fun () ->
	  ok [PushUndefined] @@
	    `Block []);
     "let should pushwith after initialize" >::
       (fun () ->
	  ok [PushString "x"; PushByte 1;
	      PushString "y"; PushByte 2;
	      NewObject 2;
	      PushWith;
	      PushUndefined;
	      PopScope] @@
       `Let ([node "x",int 1;node "y",int 2],
	     `Block []));
     "letrec should pushwith before initialize" >::
       (fun () ->
	  ok [NewObject 0;
	      Dup;
	      PushWith;
	      Dup;
	      PushByte 42;
	      SetProperty (qname "x");
	      Pop;
	      PushUndefined;
	      PopScope] @@
	    `LetRec ([node "x",int 42],`Block []));
     "lambda should use newfunction" >::
       (fun () ->
	  let m =
	    inner [] [GetLex (qname "z")] in
	    ok  [NewFunction m] @@
	      lambda [] @@ `Block [var @@ global "z"]);
     "var" >::: [
       "member should use getscope/getproperty" >::
	 (fun () ->
	    ok [GetScopeObject 1;
		GetProperty (qname "foo")] @@
	      member 1 "" "foo";
	    ok [GetScopeObject 1;
		GetProperty (qname "bar")] @@
	      member 1 "" "bar");
       "global member should use getglobal" >::
	 (fun () ->
	    ok [GetGlobalScope;
		GetProperty (qname "foo")] @@
	      global_member "" "foo");
       "slot should use GetSlot" >::
	 (fun () ->
	    ok [GetScopeObject 0;
		GetSlot 1] @@
	      slot 0 1);
       "register should use GetLocal" >::
	 (fun () ->
	    ok [GetLocal 0] @@
	      register 0;
	    ok [GetLocal 1] @@
	      register 1)
     ];
     "call" >::: [
       "call with variable should use FindPropStrict" >::
	 (fun () ->
	    ok [FindPropStrict (qname "print");
		PushString "Hello";
		CallPropLex ((qname "print"),1)] @@
	      `Call [var @@ global "print";string "Hello"]);
       "call with member should use GetScope" >::
	 (fun () ->
	    ok  [GetScopeObject 1;
		 PushByte 42;
		 CallPropLex ((qname "f"),1)] @@
	      `Call [member 1 "" "f";int 42]);
       "call with global-member should use GetGlobalScope" >::
	 (fun () ->
	    ok  [GetGlobalScope;
		 PushByte 42;
		 CallPropLex ((qname "f"),1)] @@
	      `Call [global_member "" "f"; int 42]);
       "bulidin operator shoud use it" >::
	 (fun () ->
	    ok [PushByte 1;PushByte 2;Add_i] @@
	      `Call [var @@ global "+";int 1;int 2];
	    ok [PushByte 1;PushByte 2;Equals] @@
	      `Call [var @@ global "=";int 1;int 2])
     ];
   ]) +> run_test_tt


