open Base
open OUnit
open Ast2

let ok x y =
  OUnit.assert_equal ~printer:Ast2.to_string x y

let ok_stmt x y =
  OUnit.assert_equal ~printer:Ast2.to_string_stmt x y

let expr e =
  Expr e

let block x =
  Block x

let int x =
  Int (Node.empty x)

let _ = 
  ("ast module test" >::: [
     "map" >::
       (fun () ->
	  ok (block [int 42; int 42;  block [int 42]]) @@
	    Ast2.map (function Int n -> Int {n with Node.value = 42} | e -> e) @@
	    block [int 0;
		   int 1;
		   block [int 3]]);
     "lift" >::
       (fun () ->
	  ok_stmt (expr (int 42)) @@
	    lift_stmt (Ast2.map (function Int _ -> 
				   int 42 | e -> e)) @@
	    (expr (int 10)));
   ]) +> run_test_tt
