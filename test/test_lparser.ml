open Base
open Util
open Sexp

test empty =
    assert_equal [] @@ parse_string "";
    assert_equal [] @@ parse_string "; foo bar"

test int =
    assert_equal [(Int 42)] @@ parse_string "42";
    assert_equal [(Int ~-42)] @@ parse_string "-42"

test bool =
    assert_equal [(Bool true)]  @@ parse_string "#t";
    assert_equal [(Bool false)] @@ parse_string "#f"

test float =
    assert_equal [(Float 42.)] @@ parse_string "42."

test string =
    assert_equal [(String "")] @@ parse_string "\"\"";
    assert_equal [(String "foo")] @@ parse_string "\"foo\"";
    assert_equal [(String "foo\"x")] @@ parse_string "\"foo\\\"x\"";
    assert_equal [(String "foo\"")] @@ parse_string "\"foo\\\"\"";

test symbol =
    assert_equal [(String "foo")] @@ parse_string "\"foo\"";
    assert_equal [(String "+")] @@ parse_string "\"+\""

test add =
    assert_equal [List [Symbol "+";Int 1; Int 2]] @@ parse_string "(+ 1 2)"

test list =
    assert_equal [List [Symbol "print";String "hello"]] @@ parse_string "(print \"hello\")"

test bracket_list =
    assert_equal [List [Symbol "print";String "hello"]] @@ parse_string "[print \"hello\"]"

