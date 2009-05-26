open Base
open Ast
open Scope
open AstUtil
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:Std.dump
    x (Scope.trans y)

let any = block []
let _ =
  ("scope.ml" >::: [
     "expr" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x" any;
		       expr (var ["foo"] "x")]]
	    [foo_mod [ define "x" any;
		       expr (var [] "x")]]);
     "expr(unbound)" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x" any;
		       expr (var [] "y")]]
	    [foo_mod [ define "x" any;
		       expr (var [] "y")]]);
     "expr(not module)" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x" any];
	     expr (var ["foo"] "x")]
	    [foo_mod [ define "x" any];
	     expr (var ["foo"] "x")]);
     "define" >::
       (fun () ->
	  ok
	    [foo_mod [ define "x" @@ var ["foo"] "x" ]]
	    [foo_mod [ define "x" @@ var [] "x" ]]);
     "class" >::
       (fun () ->
	  let c expr =
	    class_ (Node.ghost "Foo")
	      (Node.ghost ([],"Object")) []
	      [public_meth "f" [] expr] in
	    ok
	      [foo_mod [ define "x" any;
			 c (var ["foo"] "x")]]
	      [foo_mod [ define "x" any;
			 c (var [] "x")]])
   ]) +> run_test_tt

