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
	  ok [Int_ (node 42);
	      Int_ {(node 10) with Node.lineno=1}] "42\n10");
     "empty" >::
       (fun () ->
	  ok [] "";
	  ok [] "; foo bar");
     "int" >::
       (fun () ->
	  ok [Int_ (node 42)] "42";
	  ok [Int_ (node ~-42)] "-42");
     "bool" >::
       (fun () ->
	  ok [Bool_ (node true)]  "#t";
	  ok [Bool_ (node false)] "#f");
     "float" >::
       (fun () ->
	  ok [Float_ (node 42.)] "42.";
	  ok [Float_ (node 42.5)] "42.5");
     "string" >::
       (fun () ->
	  ok [String_ (node "")]        "\"\"";
	  ok [String_ (node "foo")]     "\"foo\"";
	  ok [String_ (node "foo\"x")]  "\"foo\\\"x\"";
	  ok [String_ (node "foo\"")]   "\"foo\\\"\"");
     "symbol" >::
       (fun () ->
	  ok [String_ (node "foo")]  "\"foo\"";
	  ok [String_ (node "+")]    "\"+\"";
	  ok [Symbol_ (node ".")]    ".");
     "+" >::
       (fun () ->
	  ok [List_ (node [Symbol_ (node "+");
			   Int_ (node 1);
			   Int_ (node 2)])] "(+ 1 2)");
     "call" >::
       (fun () ->
	  ok [List_ (node [Symbol_ (node "print");
			   String_ (node "hello")])] "(print \"hello\")");
     "bracket" >::
       (fun () ->
	  ok [List_ (node [Symbol_ (node "print");
			   String_ (node "hello")])] "[print \"hello\"]");
     "quote" >::
       (fun () ->
	  ok [List_ (node [Symbol_ (node "quote");
			   Symbol_ (node "hello")])] "(quote hello)";
	  ok [List_ (node [Symbol_ (node "quote");
			   Symbol_ (node "hello")])] "'hello")
   ]) +> run_test_tt

