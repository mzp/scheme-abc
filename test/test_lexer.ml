open Base
open Lexer
open Util
open Genlex
open OUnit
open Node

let lex str = 
  Lexer.make_lexer scheme (Stream.of_string str)

let lexer str = 
  Lexer.lexer scheme' (Node.of_string str)

let node value =
  {Node.value=value; filename="<string>"; lineno=0}

let _ =
  ("lex module test" >::: [
     "multiline" >::
       (fun () ->
	  let s =
	    lexer "x\ny" in
	    ok (node (Ident "x")) @@ Stream.next s;
	    ok {(node (Ident "y")) with Node.lineno=1} @@ Stream.next s);
     "symbol" >::
       (fun () ->
	  ok (node (Ident "+"))  @@ Stream.next (lexer "+");
	  ok (node (Ident "+.")) @@ Stream.next (lexer "+.");
	  ok (node (Ident "+.")) @@ Stream.next (lexer "+.");
	  ok (node (Ident "/"))  @@ Stream.next (lexer "/");
	  ok (node (Ident "foo.bar")) @@ Stream.next (lexer "foo.bar"));
     "dot" >::
       (fun () ->
	  ok (node (Ident ".")) @@ Stream.next (lexer "."));
     "string" >::
       (fun () ->
	  ok (node (String "")) @@ Stream.next (lexer "\"\"");
	  ok (node (String "xyz")) @@ Stream.next (lexer "\"xyz\""));
     "bool" >::
       (fun () ->
	  ok (node (Kwd "true"))  @@ Stream.next (lexer "#t");
	  ok (node (Kwd "false")) @@ Stream.next (lexer "#f"));
     "int" >::
       (fun () ->
	  ok (node (Int 42)) @@ Stream.next (lexer "42"));
     "float" >::
       (fun () ->
	  ok (node (Float 42.)) @@ Stream.next (lexer "42.");
	  ok (node (Float 42.)) @@ Stream.next (lexer "42.0");
	  ok (node (Float 42.1)) @@ Stream.next (lexer "+42.1");
	  ok (node (Float (-42.1))) @@ Stream.next (lexer "-42.1"));
     "quote" >::
       (fun () ->
	  ok (node (Kwd "'")) @@ Stream.next (lexer "'");
	  ok (node (Kwd "'")) @@ Stream.next (lexer "'hoge"))
   ]) +> run_test_tt
