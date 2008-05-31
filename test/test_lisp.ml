open Base
open Lisp
open Util
open Ast

let result xs =
  [Expr xs]

test empty =
    assert_equal [] @@ compile_string ""

test comment =
    assert_equal [] @@ 
      compile_string "; foo bar"

test string =
    assert_equal (result (String "hello")) @@ 
      compile_string "\"hello\""

test int =
    assert_equal (result (Int 42)) @@ 
      compile_string "42"

test call =
    assert_equal (result (Call ("print",[]))) @@ 
      compile_string "(print)";
    assert_equal (result (Call ("print",[String "hello"]))) @@ 
      compile_string "(print \"hello\")";
    assert_equal (result (Call ("print",[String "hello";String "world"]))) @@ 
      compile_string "(print \"hello\" \"world\")"

test add =
    assert_equal (result (Add (Int 1,Int 2))) @@ 
      compile_string "(+ 1 2)";
    assert_equal (result (Sub (Int 1,Int 2))) @@ 
      compile_string "(- 1 2)";
    assert_equal (result (Mul (Int 1,Int 2))) @@ 
      compile_string "(* 1 2)";
    assert_equal (result (Div (Int 1,Int 2))) @@ 
      compile_string "(/ 1 2)"

test predicate =
    assert_equal (result (Eq (Int 1,Int 2))) @@ 
      compile_string "(= 1 2)";
    assert_equal (result (Lt (Int 1,Int 2))) @@ 
      compile_string "(< 1 2)";
    assert_equal (result (Leq (Int 1,Int 2))) @@ 
      compile_string "(<= 1 2)";
    assert_equal (result (Gt (Int 1,Int 2))) @@ 
      compile_string "(> 1 2)";
    assert_equal (result (Geq (Int 1,Int 2))) @@ 
      compile_string "(>= 1 2)"

test if_ =
    assert_equal (result (If (Int 1,Int 2,Int 3))) @@ 
      compile_string "(if 1 2 3)"

test let_ =
    assert_equal (result (Let (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"]))) @@ 
      compile_string "(let ((x 1) (y 2)) x y)"

test begin_ =
    assert_equal (result (Block [Int 1;Int 2])) @@
      compile_string "(begin 1 2)"
      
test lammda =
    assert_equal (result (Lambda ([],Block [Int 42]))) @@
      compile_string "(lambda () 42)"

test lammda_with_args =
    assert_equal (result (Lambda (["a";"b";"c"],Block [Int 42]))) @@
      compile_string "(lambda (a b c) 42)"
