open Base
open Ast
open Util
open ClosureTrans
open AstUtil
open OUnit

let compile_string str =
  ClosTrans.trans @@ Lisp.compile_string str

let ok x y =
  OUnit.assert_equal
    x y

let _ =
  ("closure trans" >::: [
     "arguments" >::
       (fun () ->
	  let lambda =
	    `Lambda ([sname "x"],
		     `Let ([sname "x",`Var (global "x")],
			   `Lambda ([],
				    `Var (global "x")))) in
	    ok [define (`Public (global "f"))  lambda] @@
	      trans @@ [define (`Public (global "f")) @@
			  `Lambda ([sname "x"],
				   `Lambda ([],
					    `Var (global "x")))]);
     "class" >::
       (fun () ->
	  ok [
	    klass (`Public (global "Foo")) (global "Object") []
	      [public_meth "init" ["self"] @@
		 `Let ([sname "self",`Var (global "self")],
		       `Lambda ([],`Var (global "self")))]] @@
	    trans @@ [klass (`Public (global "Foo")) (global "Object") []
			[public_meth "init" ["self"] @@
			   `Lambda ([],`Var (global "self"))]])

   ]) +> run_test_tt
