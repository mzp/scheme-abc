open Base
open Lisp
open Util
open Ast

test empty =
    assert_equal [] @@ compile_string ""

test string =
    assert_equal [String "hello"] @@ compile_string "\"hello\""

test call =
    assert_equal [Call ("print",[])] @@ compile_string "(print)";
    assert_equal [Call ("print",[String "hello"])] @@ compile_string "(print \"hello\")";
    assert_equal [Call ("print",[String "hello";String "world"])] @@ 
      compile_string "(print \"hello\" \"world\")"

