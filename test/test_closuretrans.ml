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
	    ok [`Define (global "f",lambda)] @@
	      trans @@ [`Define (global "f",
				 `Lambda ([sname "x"],
					  `Lambda ([],
						   `Var (global "x"))))]);
     "class" >::
       (fun () ->
	  ok [
	    `Class (global "Foo",sname ("","Object"),[],
		    [sname "init",[sname "self"],
		     `Let ([sname "self",`Var (global "self")],
			   `Lambda ([],`Var (global "self")))])] @@
	    trans @@ [`Class (global "Foo",sname ("","Object"),[],
			      [sname "init",[sname "self"],
			       `Lambda ([],`Var (global "self"))])])

   ]) +> run_test_tt
