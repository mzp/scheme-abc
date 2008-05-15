open Base
open Lexer
open Util

let lex str = 
  make_lexer scheme (Stream.of_string str)

test symbol =
  assert_equal (Genlex.Ident "+") @@ Stream.next (lex "+")
