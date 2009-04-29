open Base
let parse methods stream =
  stream
  +> Lisp.parse
  +> Clos.to_ast methods


let parse_string methods string =
  string
  +> Lisp.parse_string
  +> Clos.to_ast methods

