open Base
open Lexer
open Util
open Genlex
open OUnit

let lex str = 
  Lexer.make_lexer scheme (Stream.of_string str)

let _ =
  ("lex module test" >::: [
     "symbol" >::
       (fun () ->
	  ok (Ident "+")  @@ Stream.next (lex "+");
	  ok (Ident "+.") @@ Stream.next (lex "+.");
	  ok (Ident "+.") @@ Stream.next (lex "+.");
	  ok (Ident "foo.bar") @@ Stream.next (lex "foo.bar"));
     "dot" >::
       (fun () ->
	  ok (Ident ".") @@ Stream.next (lex "."));
     "bool" >::
       (fun () ->
	  ok (Kwd "true")  @@ Stream.next (lex "#t");
	  ok (Kwd "false") @@ Stream.next (lex "#f"));
     "int" >::
       (fun () ->
	  ok (Int 42) @@ Stream.next (lex "42"));
     "float" >::
       (fun () ->
	  ok (Float 42.) @@ Stream.next (lex "42.");
	  ok (Float 42.) @@ Stream.next (lex "42.0");
	  ok (Float 42.1) @@ Stream.next (lex "+42.1");
	  ok (Float (-42.1)) @@ Stream.next (lex "-42.1"));
     "quote" >::
       (fun () ->
	  ok (Kwd "'") @@ Stream.next (lex "'");
	  ok (Kwd "'") @@ Stream.next (lex "'hoge"))
   ]) +> run_test_tt
