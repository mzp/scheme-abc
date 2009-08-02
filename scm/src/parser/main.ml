open Base
let parse _ stream =
  stream
  +> Lexer.lexer Lexer.scheme
  +> Sexp.of_stream
  +> Lisp.parse

let parse_string _ string =
  string
  +> Node.of_string
  +> parse


