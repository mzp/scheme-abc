open Base
let parse _ stream =
  stream
  +> Lisp.parse

let parse_string _ string =
  string
  +> Lisp.parse_string


