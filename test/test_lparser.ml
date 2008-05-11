open Base
open Util
open Lparser

test empty =
    assert_equal [] @@ parse_string "";
    assert_equal [] @@ parse_string "; foo bar"

test int =
    assert_equal [(Int 42)] @@ parse_string "42";
    assert_equal [(Int ~-42)] @@ parse_string "-42"

test string =
    assert_equal [(String "")] @@ parse_string "\"\"";
    assert_equal [(String "foo")] @@ parse_string "\"foo\"";
    assert_equal [(String "foo\"x")] @@ parse_string "\"foo\\\"x\"";
    assert_equal [(String "foo\"")] @@ parse_string "\"foo\\\"\"";

test symbol =
    assert_equal [(String "foo")] @@ parse_string "\"foo\"";
    assert_equal [(String "+")] @@ parse_string "\"+\""

test list =
    assert_equal [List [Symbol "print";String "hello"]] @@ parse_string "(print \"hello\")"
