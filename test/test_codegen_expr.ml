open Base
open Asm
open Ast
open Cpool
open Codegen
open VarResolve
open Util
open OUnit
open AstUtil

(** util function *)
let string_of_insts xs =
  let ys =
    String.concat "; \n\t" @@ List.map string_of_instruction xs in
    Printf.sprintf "[\n\t%s ]\n" ys

let expr inst =
  {Asm.empty_method with
     name =
      make_qname "";
     instructions=
      [GetLocal_0;PushScope]@inst@[Pop;ReturnVoid]}

let compile x =
  generate_script @@ [`Expr x]

let ok l r =
  let lhs =
    expr l in
  let rhs =
    compile r in
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

let member i name =
  `BindVar (node (Member ((Scope i),name)))

let global_member name =
  `BindVar (node (Member (Global,name)))

let slot i j =
  `BindVar (node (Slot ((Scope i),j)))

let register i =
  `BindVar (node (Register i))

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

let qname =
  Cpool.make_qname

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
	       IfNe a; PushByte 0; Jump b;
	       Asm.Label a;PushByte 1; Asm.Label b] @@
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
	  ok  [NewFunction (inner [] [GetLex (qname "z")])] @@
	    lambda [] @@ `Block [var @@ global "z"]);
     "var" >::: [
       "member should use getscope/getproperty" >::
	 (fun () ->
	    ok [GetScopeObject 1;
		GetProperty (qname "foo")] @@
	      member 1 "foo";
	    ok [GetScopeObject 1;
		GetProperty (qname "bar")] @@
	      member 1 "bar");
       "global member should use getglobal" >::
	 (fun () ->
	    ok [GetGlobalScope;
		GetProperty (qname "foo")] @@
	      global_member "foo");
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
	      `Call [member 1 "f";int 42]);
       "call with global-member should use GetGlobalScope" >::
	 (fun () ->
	    ok  [GetGlobalScope;
		 PushByte 42;
		 CallPropLex ((qname "f"),1)] @@
	      `Call [global_member "f";int 42]);
       "bulidin operator shoud use it" >::
	 (fun () ->
	    ok [PushByte 1;PushByte 2;Add_i] @@
	      `Call [var @@ global "+";int 1;int 2];
	    ok [PushByte 1;PushByte 2;Equals] @@
	      `Call [var @@ global "=";int 1;int 2])
     ];
   ]) +> run_test_tt


