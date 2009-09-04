open Base
open Node
open Sexp
open OUnit

let pos x n a b =
  {(Node.ghost x) with
     Node.filename = "<string>";
     lineno        = n;
     start_pos     = a;
     end_pos       = b}

let of_tokens tokens =
  Sexp.of_stream @@ Stream.of_list tokens

let ok sexp tokens =
  let sexp' =
    of_tokens tokens in
    OUnit.assert_equal
      sexp
      sexp'

let node x =
  pos x 1 2 3

let int n =
  Int (node n)
let string s =
  String (node s)
let bool b =
  Bool (node b)
let float f =
  Float (node f)
let symbol s =
  Symbol (node s)
let list l =
  List (node l)

let t_int n =
  node (Genlex.Int n)

let t_float n =
  node (Genlex.Float n)

let t_str str =
  node (Genlex.String str)

let t_char c =
  node (Genlex.String c)

let t_ident s =
  node (Genlex.Ident s)

let t_kwd s =
  node (Genlex.Kwd s)

let _ =
  ("sexp.ml" >::: [
     "pos" >::
       (fun () ->
	  assert_equal
	    [List   (pos [Symbol (pos "a" 5 1 2);
			  Symbol (pos "b" 5 3 4);
			  Symbol (pos "c" 5 5 6)] 5 0 7)] @@
	    of_tokens [
	      pos (Genlex.Kwd "(")      5 0 1;
	        pos (Genlex.Ident "a")  5 1 2;
		pos (Genlex.Ident "b")  5 3 4;
		pos (Genlex.Ident "c")  5 5 6;
	      pos (Genlex.Kwd ")")      5 6 7;
	    ]);
     "empty" >::
       (fun () ->
	  ok [] []);
     "int" >::
       (fun () ->
	  ok [int 42]   [t_int 42];
	  ok [int ~-42] [t_int (~-42)]);
     "bool" >::
       (fun () ->
	  ok [bool true]  [t_kwd "true"];
	  ok [bool false] [t_kwd "false"]);
     "float" >::
       (fun () ->
	  ok [float 42.1]  [t_float (42.1)]);
     "string" >::
       (fun () ->
	  ok [string ""]       [t_str ""];
	  ok [string "foo"]    [t_str "foo"];
	  ok [string "foo\"x"] [t_str "foo\"x"];
	  ok [string "foo\""]  [t_str "foo\""]);
     "symbol" >::
       (fun () ->
	  ok [symbol "."]  [t_ident "."]);
     "call" >::
       (fun () ->
	  ok [list [symbol "+"; int 1; int 2]]
	     [t_kwd "(";
	      t_ident "+"; t_int 1; t_int 2;
	      t_kwd ")"];
	  ok [list [symbol "print"; string "hello"]]
	     [t_kwd "(";
	      t_ident "print"; t_str "hello";
	      t_kwd ")"]);
     "bracket" >::
       (fun () ->
	  ok [list [symbol "print"; string "hello"]]
	    [t_kwd "[";
	      t_ident "print"; t_str "hello";
	      t_kwd "]"]);
     "quote" >::
       (fun () ->
	  ok [list [symbol "quote"; symbol "hello"]]
	     [t_kwd "(";
	      t_ident "quote"; t_ident "hello";
	      t_kwd ")"];
	  ok [list [symbol "quote"; symbol "hello"]]
	    [t_kwd "'"; t_ident "hello"])
   ]) +> run_test_tt_main
