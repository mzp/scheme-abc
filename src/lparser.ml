(* lisp parser *)
open Base
type lisp = Int of int | String of string | Symbol of string | List of lisp list

let rec read =
  parser
      [<'Genlex.String s >] -> String s
    | [<'Genlex.Ident name >] -> Symbol name
    | [<'Genlex.Int n >] -> Int n
    | [<'Genlex.Kwd "("; c = Parsec.many read; 'Genlex.Kwd ")" >] -> List c

let lexer =
  Lexer.make_lexer Lexer.scheme

let parse stream =
  Parsec.many read @@ lexer stream

let parse_string string =
  parse @@ Stream.of_string string
