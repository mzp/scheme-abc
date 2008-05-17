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
    assert_equal [Gt (Int 1,Int 2)] @@ compile_string "(< 1 2)";
    assert_equal [Geq (Int 1,Int 2)] @@ compile_string "(<= 1 2)";
    assert_equal [Lt (Int 1,Int 2)] @@ compile_string "(> 1 2)";
    assert_equal [Leq (Int 1,Int 2)] @@ compile_string "(>= 1 2)"

test if_ =
    assert_equal [If (Int 1,Int 2,Int 3)] @@ compile_string "(if 1 2 3)"
