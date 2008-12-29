open Base
open Asm
open Ast
open Cpool
open Codegen
open Util
open OUnit
open AstUtil

let ok_s expect actual =
  assert_equal expect @@ generate_program [actual]

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

let _ =
  ("codegen.ml(stmt)" >::: [
     "redefine should use setproperty" >::
       (fun () ->
	  ok_s [PushByte 42;
		GetScopeObject 0;
		Swap;
		SetProperty (qname "f")] @@
	    redefine (`Public (global "f")) 0 (int 42));
     "define should use PushWith" >::
       (fun () ->
	  ok_s [FindPropStrict (qname "$Scope");
		ConstructProp (qname "$Scope",0);
		Dup;PushWith;
		PushByte 42;
		SetProperty (qname "f")] @@
	    define (`Public (global "f")) (int 42));
   ]) +> run_test_tt


