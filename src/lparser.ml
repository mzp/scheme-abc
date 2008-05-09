(* lisp parser *)
open Base
type lisp = String of string | Symbol of string | List of lisp list

let rec parse =
  parser
      [<'Genlex.String s >] -> String s
    | [<'Genlex.Ident name >] -> Symbol name
    | [<'Genlex.Kwd "("; c = Parsec.many parse; 'Genlex.Kwd ")" >] -> List c

let lexer =
  Lexer.make_lexer Lexer.scheme
  
let read stream =
  Parsec.many parse @@ lexer stream

let read_string string =
  read @@ Stream.of_string string
