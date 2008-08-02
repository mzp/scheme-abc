open Base
open Sexp

let assert_equal sexp str =
  let sexp' =
    parse_string str in
    OUnit.assert_equal 
      ~printer:(String.concat ";\n" $ List.map Sexp.to_string) 
      sexp 
      sexp'
  
test empty =
    assert_equal [] "";
    assert_equal [] "; foo bar"

test int =
    assert_equal [(Int 42)] "42";
    assert_equal [(Int ~-42)] "-42"

test bool =
    assert_equal [(Bool true)]  "#t";
    assert_equal [(Bool false)] "#f"

test float =
    assert_equal [(Float 42.)] "42."

test string =
    assert_equal [(String "")]        "\"\"";
    assert_equal [(String "foo")]     "\"foo\"";
    assert_equal [(String "foo\"x")]  "\"foo\\\"x\"";
    assert_equal [(String "foo\"")]   "\"foo\\\"\"";

test symbol =
    assert_equal [(String "foo")]  "\"foo\"";
    assert_equal [(String "+")]    "\"+\""

test add =
    assert_equal [List [Symbol "+";Int 1; Int 2]] "(+ 1 2)"

test list =
    assert_equal [List [Symbol "print";String "hello"]] "(print \"hello\")"

test bracket_list =
    assert_equal [List [Symbol "print";String "hello"]] "[print \"hello\"]"

test quote =
    assert_equal [List [Symbol "quote";Symbol "hello"]] "(quote hello)";
    assert_equal [List [Symbol "quote";Symbol "hello"]] "'hello";

