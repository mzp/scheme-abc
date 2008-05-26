open Base
open Lisp
open Util
open Ast

test empty =
    assert_equal [] @@ compile_string ""

test string =
    assert_equal [String "hello"] @@ compile_string "\"hello\""

test comment =
    assert_equal [] @@ compile_string "; foo bar"

test int =
    assert_equal [Int 42] @@ compile_string "42"

test call =
    assert_equal [Call ("print",[])] @@ compile_string "(print)";
    assert_equal [Call ("print",[String "hello"])] @@ compile_string "(print \"hello\")";
    assert_equal [Call ("print",[String "hello";String "world"])] @@ 
      compile_string "(print \"hello\" \"world\")"

test add =
    assert_equal [Add (Int 1,Int 2)] @@ compile_string "(+ 1 2)";
    assert_equal [Sub (Int 1,Int 2)] @@ compile_string "(- 1 2)";
    assert_equal [Mul (Int 1,Int 2)] @@ compile_string "(* 1 2)";
    assert_equal [Div (Int 1,Int 2)] @@ compile_string "(/ 1 2)"

test eq =
    assert_equal [Eq (Int 1,Int 2)] @@ compile_string "(= 1 2)";
    assert_equal [Lt (Int 1,Int 2)] @@ compile_string "(< 1 2)";
    assert_equal [Leq (Int 1,Int 2)] @@ compile_string "(<= 1 2)";
    assert_equal [Gt (Int 1,Int 2)] @@ compile_string "(> 1 2)";
    assert_equal [Geq (Int 1,Int 2)] @@ compile_string "(>= 1 2)"

test if_ =
    assert_equal [If (Int 1,Int 2,Int 3)] @@ compile_string "(if 1 2 3)"

test let_ =
    assert_equal [Let (["x",Int 1;"y",Int 2],Block [Var "x";Var "y"])] @@ 
      compile_string "(let ((x 1) (y 2)) x y)"

test begin_ =
    assert_equal [Block [Int 1;Int 2]] @@
      compile_string "(begin 1 2)"
      
test lammda =
    assert_equal [Method ("",[],Block [Int 42])] @@
      compile_string "(lambda () 42)"

test lammda_with_args =
    assert_equal [Method ("",["a";"b";"c"],Block [Int 42])] @@
      compile_string "(lambda (a b c) 42)"
