open Base
open Sexp
open OUnit

let ok sexp str =
  let sexp' =
    of_string str in
    OUnit.assert_equal
      ~printer:(String.concat ";\n" $ List.map Sexp.to_string) 
      sexp 
      sexp'

let node x =
  {Node.value = x; filename = "<string>"; lineno = 0}

let _ =
  ("S expression module test" >::: [
     "multi line" >::
       (fun () ->
	  ok [Int (node 42);
	      Int {(node 10) with Node.lineno=1}] "42\n10");
     "empty" >::
       (fun () ->
	  ok [] "";
	  ok [] "; foo bar");
     "int" >::
       (fun () ->
	  ok [Int (node 42)] "42";
	  ok [Int (node ~-42)] "-42");
     "bool" >::
       (fun () ->
	  ok [Bool (node true)]  "#t";
	  ok [Bool (node false)] "#f");
     "float" >::
       (fun () ->
	  ok [Float (node 42.)] "42.";
	  ok [Float (node 42.5)] "42.5");
     "string" >::
       (fun () ->
	  ok [String (node "")]        "\"\"";
	  ok [String (node "foo")]     "\"foo\"";
	  ok [String (node "foo\"x")]  "\"foo\\\"x\"";
	  ok [String (node "foo\"")]   "\"foo\\\"\"");
     "symbol" >::
       (fun () ->
	  ok [String (node "foo")]  "\"foo\"";
	  ok [String (node "+")]    "\"+\"";
	  ok [Symbol (node ".")]    ".");
     "+" >::
       (fun () ->
	  ok [List (node [Symbol (node "+");
			   Int (node 1);
			   Int (node 2)])] "(+ 1 2)");
     "call" >::
       (fun () ->
	  ok [List (node [Symbol (node "print");
			   String (node "hello")])] "(print \"hello\")");
     "bracket" >::
       (fun () ->
	  ok [List (node [Symbol (node "print");
			   String (node "hello")])] "[print \"hello\"]");
     "quote" >::
       (fun () ->
	  ok [List (node [Symbol (node "quote");
			   Symbol (node "hello")])] "(quote hello)";
	  ok [List (node [Symbol (node "quote");
			   Symbol (node "hello")])] "'hello")
   ]) +> run_test_tt

