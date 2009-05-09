open Base
open Node
open Sexp
open OUnit

let pos x n a b =
  {(Node.empty x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

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

let int n =
  Int (Node.ghost n)
let string s =
  String (Node.ghost s)
let bool b =
  Bool (Node.ghost b)
let float f =
  Float (Node.ghost f)
let symbol s =
  Symbol (Node.ghost s)
let list l =
  List (Node.ghost l)

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
     "empty" >::
       (fun () ->
	  ok [] "";
	  ok [] "; foo bar");
     "int" >::
       (fun () ->
	  ok [int 42]   "42";
	  ok [int ~-42] "-42");
     "bool" >::
       (fun () ->
	  ok [bool true]  "#t";
	  ok [bool false] "#f");
     "float" >::
       (fun () ->
	  ok [float 42.]  "42.";
	  ok [float 42.5] "42.5");
     "string" >::
       (fun () ->
	  ok [string ""]       "\"\"";
	  ok [string "foo"]    "\"foo\"";
	  ok [string "foo\"x"] "\"foo\\\"x\"";
	  ok [string "foo\""]  "\"foo\\\"\"");
     "symbol" >::
       (fun () ->
	  ok [string "foo"] "\"foo\"";
	  ok [string "+"]   "\"+\"";
	  ok [symbol "."]   ".");
     "+" >::
       (fun () ->
	  ok [list [symbol "+"; int 1; int 2]]
	    "(+ 1 2)");
     "call" >::
       (fun () ->
	  ok [list [symbol "print"; string "hello"]]
	    "(print \"hello\")");
     "bracket" >::
       (fun () ->
	  ok [list [symbol "print"; string "hello"]]
	    "[print \"hello\"]");
     "quote" >::
       (fun () ->
	  ok [list [symbol "quote"; symbol "hello"]]
	    "(quote hello)";
	  ok [list [symbol "quote"; symbol "hello"]]
	    "'hello")
   ]) +> run_test_tt

