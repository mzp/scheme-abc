open Base
open Lexer
open Genlex
open OUnit
open Node

let lexer str =
  Lexer.lexer scheme (Node.of_string str)

let pos value line a b =
  {(Node.ghost value) with
     Node.filename =  "<string>";
     lineno        = line;
     start_pos     = a;
     end_pos       = b}

let ok a {Node.value = b} =
  assert_equal a b

let token str =
  Stream.next @@ lexer str

let _ =
  ("lex module test" >::: [
     "position" >::
       (fun () ->
	  let s =
	    lexer "(
foo
\"abc\"
42
42.0
#t #f" in
	    assert_equal (pos (Kwd "(")      0 0 1) @@ Stream.next s;
	    assert_equal (pos (Ident "foo")  1 0 3) @@ Stream.next s;
	    assert_equal (pos (String "abc") 2 0 5) @@ Stream.next s;
	    assert_equal (pos (Int 42)       3 0 2) @@ Stream.next s;
	    assert_equal (pos (Float 42.0)   4 0 4) @@ Stream.next s;
	    assert_equal (pos (Kwd "true")   5 0 2) @@ Stream.next s;
	    assert_equal (pos (Kwd "false")  5 3 5) @@ Stream.next s);
     "multiline" >::
       (fun () ->
	  let s =
	    lexer "x\ny" in
	    ok (Ident "x") @@ Stream.next s;
	    ok (Ident "y") @@ Stream.next s);
     "symbol" >::
       (fun () ->
	  ok (Ident "+")  @@ token "+";
	  ok (Ident "$")  @@ token "$";
	  ok (Ident "a$")  @@ token "a$";
	  ok (Ident "+.") @@ token "+.";
	  ok (Ident "+.") @@ token "+.";
	  ok (Ident "/")  @@ token "/";
	  ok (Ident "foo.bar") @@ token "foo.bar");
     "dot" >::
       (fun () ->
	  ok (Ident ".") @@ token ".");
     "string" >::
       (fun () ->
	  ok (String "") @@ token "\"\"";
	  ok (String "xyz") @@ token "\"xyz\"");
     "bool" >::
       (fun () ->
	  ok (Kwd "true")  @@ token "#t";
	  ok (Kwd "false") @@ token "#f");
     "int" >::
       (fun () ->
	  ok (Int 42) @@ token "42");
     "hex" >::
       (fun () ->
	  ok (Int 0x42) @@ token "0x42";
	  ok (Int (-0x42)) @@ token "-0x42");
     "float" >::
       (fun () ->
	  ok (Float 42.) @@ token "42.";
	  ok (Float 42.) @@ token "42.0";
	  ok (Float 42.1) @@ token "+42.1";
	  ok (Float (-42.1)) @@ token "-42.1");
     "quote" >::
       (fun () ->
	  ok (Kwd "'") @@ token "'";
	  ok (Kwd "'") @@ token "'hoge")
   ]) +> run_test_tt
