open Base
open Lexer
open Genlex
open OUnit
open Node

let lexer str = 
  Lexer.lexer scheme (Node.of_string str)

let node value =
  {(Node.empty value) with Node.filename= "<string>"}

let ok a {Node.value = b} =
  Util.ok a b

let _ =
  ("lex module test" >::: [
     "multiline" >::
       (fun () ->
	  let s =
	    lexer "x\ny" in
	    ok (Ident "x") @@ Stream.next s;
	    ok (Ident "y") @@ Stream.next s);
     "position" >::
       (fun () ->
	  let s =
	    lexer "\"abc\"\n42" in
	    Util.ok {(node (String "abc")) with 
		       start_pos = 0;
		       end_pos   = 5} @@ Stream.next s;
	    Util.ok {(node (Int 42))  with end_pos = 2; lineno=1} @@ Stream.next s);
     "symbol" >::
       (fun () ->
	  ok (Ident "+")  @@ Stream.next (lexer "+");
	  ok (Ident "+.") @@ Stream.next (lexer "+.");
	  ok (Ident "+.") @@ Stream.next (lexer "+.");
	  ok (Ident "/")  @@ Stream.next (lexer "/");
	  ok (Ident "foo.bar") @@ Stream.next (lexer "foo.bar"));
     "dot" >::
       (fun () ->
	  ok (Ident ".") @@ Stream.next (lexer "."));
     "string" >::
       (fun () ->
	  ok (String "") @@ Stream.next (lexer "\"\"");
	  ok (String "xyz") @@ Stream.next (lexer "\"xyz\""));
     "bool" >::
       (fun () ->
	  ok (Kwd "true")  @@ Stream.next (lexer "#t");
	  ok (Kwd "false") @@ Stream.next (lexer "#f"));
     "int" >::
       (fun () ->
	  ok (Int 42) @@ Stream.next (lexer "42"));
     "float" >::
       (fun () ->
	  ok (Float 42.) @@ Stream.next (lexer "42.");
	  ok (Float 42.) @@ Stream.next (lexer "42.0");
	  ok (Float 42.1) @@ Stream.next (lexer "+42.1");
	  ok (Float (-42.1)) @@ Stream.next (lexer "-42.1"));
     "quote" >::
       (fun () ->
	  ok (Kwd "'") @@ Stream.next (lexer "'");
	  ok (Kwd "'") @@ Stream.next (lexer "'hoge"))
   ]) +> run_test_tt
