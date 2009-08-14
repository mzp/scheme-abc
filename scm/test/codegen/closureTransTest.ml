open Base
open Ast
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
	      lambda ["x"]
		(let_ ["x",var [] "x"]
		   (lambda ["y"]
		      (var [] "x")))] in
	    ok [`Define (`Public (Node.ghost ([],"f")),body)] @@
	      trans @@ [`Define (`Public (Node.ghost ([],"f")),
				 block [
				   lambda ["x"] @@
				     lambda ["y"] @@
				     var [] "x"])]);
     "class" >::
       (fun () ->
	  ok
	    [`Class{
	       Ast.class_name = `Public (Node.ghost ([],"Foo"));
	       super =  Node.ghost ([],"Object");
	       attrs = [];
	       methods = [
		 public_meth "init" ["self"] @@
		   let_ ["self",var [] "self"] @@
		     lambda [] (var [] "self")]}] @@
	    trans @@ [`Class{
			Ast.class_name = `Public (Node.ghost ([],"Foo"));
			super =  Node.ghost ([],"Object");
			attrs = [];
			methods = [public_meth "init" ["self"] @@
				     lambda [] (var [] "self")]}])
   ]) +> run_test_tt
