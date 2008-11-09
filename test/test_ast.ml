open Base
open Ast
open OUnit

let ok x y =
  OUnit.assert_equal ~printer:Ast.to_string x y

let ok_stmt x y =
  OUnit.assert_equal ~printer:Ast.to_string_stmt x y

let _ = ("ast module test" >::: [
  "map" >::
    (fun () ->
       ok (Block [Int 42;Int 42;Block [Int 42]]) @@
	 Ast.map (function Int _ -> Int 42 | e -> e) @@
	 Block [Int 0; Int 1; Block [Int 3]]);
  "lift" >::
    (fun () ->
       ok_stmt (Expr (Int 42)) @@
	 lift_stmt (Ast.map (function Int _ -> Int 42 | e -> e)) @@
	 Ast.Expr (Ast.Int 10))
	 ]) +> run_test_tt
