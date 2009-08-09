open Base
open Asm
open Ast
open Codegen
open OUnit
open AstUtil
open ISpec

let ok_s expect actual =
  assert_equal expect @@ generate_program [actual]

let define x expr =
  `Define (x,expr)

let redefine x n expr =
  `ReDefine (x,n,expr)

let join xs =
  String.concat "." xs

let qname ns x =
  `QName ((`Namespace (join ns)),x)

let _ =
  ("codegen.ml(stmt)" >::: [
     "redefine" >::
       (fun () ->
	  ok_s [`PushByte 42;
		`GetScopeObject 0;
		`Swap;
		`SetProperty (qname [] "f")] @@
	    redefine (`Public (AstUtil.qname [] "f")) 0 (int 42));
     "define" >::
       (fun () ->
	  ok_s [`FindPropStrict (qname [] "$Scope");
		`ConstructProp (qname [] "$Scope",0);
		`Dup; `PushWith;
		`PushByte 42;
		`SetProperty (qname [] "f")] @@
	    define (`Public (AstUtil.qname [] "f")) (int 42));
   ]) +> run_test_tt


