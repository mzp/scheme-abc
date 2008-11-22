open Base
open Lexer
open Genlex
open OUnit
open Node

let lexer str = 
  Lexer.lexer scheme (Node.of_string str)

let node value line a b =
  {(Node.empty value) with 
     Node.filename =  "<string>";
     lineno        = line;
     start_pos     = a;
     end_pos       = b}

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
	    lexer "(
foo
\"abc\"
42
42.0" in
	    Util.ok (node (Kwd "(")      0 0 1) @@ Stream.next s;
	    Util.ok (node (Ident "foo")  1 0 3) @@ Stream.next s;
	    Util.ok (node (String "abc") 2 0 5) @@ Stream.next s;
	    Util.ok (node (Int 42)       3 0 2) @@ Stream.next s;
	    Util.ok (node (Float 42.0)   4 0 4) @@ Stream.next s);
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
