open Base
open Node
open Sexp
open OUnit

let rec eq lhs rhs =
    match lhs,rhs with
	Int {value=x}, Int {value=y} ->
	  x = y
      | String {value=x}, String {value=y} ->
	  x = y
      | Float  {value=x}, Float {value=y} ->
	  x = y
      | Bool   {value=x}, Bool  {value=y} ->
	  x = y
      | Symbol {value=x}, Symbol  {value=y} ->
	  x = y
      | List   {value=x}, List  {value=y} ->
	  List.for_all2 eq x y
      | _ ->
	  false

let ok sexp str =
  let sexp' =
    of_string str in
    OUnit.assert_equal
      ~cmp:(fun a b -> List.for_all2 eq a b)
      ~printer:(String.concat ";\n" $ List.map Sexp.to_string)
      sexp
      sexp'

let node x =
  {(Node.empty x) with Node.filename= "<string>"}

let pos x n a b =
  {(Node.empty x) with 
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let _ =
  ("S expression module test" >::: [
     "pos" >::
       (fun () ->
	  assert_equal 
	    ~printer:(String.concat ";\n" $ List.map Sexp.to_string)
	    [Int    (pos 42    0 0 2);
	     String (pos "str" 1 0 5);
	     Float  (pos 42.0  2 0 4);
	     Bool   (pos true  3 0 2);
	     Bool   (pos false 3 3 5);
	     Symbol (pos "hoge" 4 0 4);
	     List   (pos [Symbol (pos "a" 5 1 2);
			  Symbol (pos "b" 5 3 4);
			  Symbol (pos "c" 5 5 6)] 5 0 7)] @@
	    of_string "42
\"str\"
42.0
#t #f
hoge
(a b c)");
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

