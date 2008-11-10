open Base
open Sexp
open OUnit

let ok sexp str =
  let sexp' =
    parse_string str in
    OUnit.assert_equal
      ~printer:(String.concat ";\n" $ List.map Sexp.to_string) 
      sexp 
      sexp'

let _ =
  ("S expression module test" >::: [
     "empty" >::
       (fun () ->
	  ok [] "";
	  ok [] "; foo bar");
     "int" >::
       (fun () ->
	  ok [(Int 42)] "42";
	  ok [(Int ~-42)] "-42");
     "bool" >::
       (fun () ->
	  ok [(Bool true)]  "#t";
	  ok [(Bool false)] "#f");
     "float" >::
       (fun () ->
	  ok [(Float 42.)] "42.";
	  ok [(Float 42.5)] "42.5");
     "string" >::
       (fun () ->
	  ok [(String "")]        "\"\"";
	  ok [(String "foo")]     "\"foo\"";
	  ok [(String "foo\"x")]  "\"foo\\\"x\"";
	  ok [(String "foo\"")]   "\"foo\\\"\"");
     "symbol" >::
       (fun () ->
	  ok [(String "foo")]  "\"foo\"";
	  ok [(String "+")]    "\"+\"";
	  ok [(Symbol ".")]    ".");
     "+" >::
       (fun () ->
	  ok [List [Symbol "+";Int 1; Int 2]] "(+ 1 2)");
     "call" >::
       (fun () ->
	  ok [List [Symbol "print";String "hello"]] "(print \"hello\")");
     "bracket" >::
       (fun () ->
	  ok [List [Symbol "print";String "hello"]] "[print \"hello\"]");
     "quote" >::
       (fun () ->
	  ok [List [Symbol "quote";Symbol "hello"]] "(quote hello)";
	  ok [List [Symbol "quote";Symbol "hello"]] "'hello")
   ]) +> run_test_tt

