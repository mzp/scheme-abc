open Base
open Ast

let assert_equal x y =
  OUnit.assert_equal ~printer:Ast.to_string x y

let const42 =
  function
      Ast.Int _ ->
	Int 42
    | e ->
	e

test map =
  let mapped = 
    Ast.map const42 @@ Block [Int 0; Int 1; Block [Int 3]]  in
    assert_equal (Block [Int 42;Int 42;Block [Int 42]]) mapped

test if_ =
  let mapped = 
    Ast.map const42 @@ If (Int 0, Int 1, Int 2) in
    assert_equal (If (Int 42,Int 42,Int 42)) mapped

test lift =
  let f stmt =
    lift_stmt (Ast.map const42) stmt in
  let stmt =
    Ast.Expr (Ast.Int 10) in
    OUnit.assert_equal (Expr (Int 42)) (f stmt)
