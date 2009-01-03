open Base
open Ast
open Util
open ClosureTrans
open AstUtil
open OUnit

let ok x y =
  OUnit.assert_equal
    x y

let _ =
  ("closureTrans.ml" >::: [
     "arguments" >::
       (fun () ->
	  let body =
	    block [
	      lambda ["x"] @@
		`Let ([sname "x",var @@ global "x"],
		      lambda ["y"] @@
			var @@ global "x")] in
	    ok [define (`Public (global "f"))  @@ body] @@
	      trans @@ [define (`Public (global "f")) @@
			  block [
			    lambda ["x"] @@
			      lambda ["y"] @@
			      var @@ global "x"]]);
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
