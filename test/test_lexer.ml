open Base
open Lexer
open Util
open Genlex

let lex str = 
  Lexer.make_lexer scheme (Stream.of_string str)

test symbol =
  assert_equal (Ident "+") @@ Stream.next (lex "+");
  assert_equal (Ident "+.") @@ Stream.next (lex "+.")

test bool =
  assert_equal (Kwd "true")  @@ Stream.next (lex "#t");
  assert_equal (Kwd "false") @@ Stream.next (lex "#f")

test int =
  assert_equal (Int 42) @@ Stream.next (lex "42")

test float =
  assert_equal (Float 42.) @@ Stream.next (lex "42.");
  assert_equal (Float 42.) @@ Stream.next (lex "42.0");
  assert_equal (Float 42.1) @@ Stream.next (lex "+42.1");
  assert_equal (Float (-42.1)) @@ Stream.next (lex "-42.1")
