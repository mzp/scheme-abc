(* lisp parser *)
open Base
type lisp = 
    Int    of int 
  | String of string 
  | Float  of float 
  | Bool   of bool 
  | Symbol of string 
  | List   of lisp list

let rec to_string =
  function
      Int    n ->
	string_of_int n
    | String s ->
	Printf.sprintf "\"%s\"" s
    | Float  d ->
	string_of_float d
    | Symbol s ->
	s
    | Bool   b ->
	if b then "#t" else "#f"
    | List   xs ->
	let s =
	  String.concat " " @@ List.map to_string xs in
	  Printf.sprintf "(%s)" s

let rec read =
  parser
      [<'Genlex.String s >] -> String s
    | [<'Genlex.Ident name >] -> Symbol name
    | [<'Genlex.Int n >] -> Int n
    | [<'Genlex.Float x>] -> Float x
    | [<'Genlex.Kwd "true" >] -> Bool true
    | [<'Genlex.Kwd "false" >] -> Bool false
    | [<'Genlex.Kwd "("; c = Parsec.many read; 'Genlex.Kwd ")" >] -> List c
    | [<'Genlex.Kwd "["; c = Parsec.many read; 'Genlex.Kwd "]" >] -> List c
    | [<'Genlex.Kwd "'"; c = Parsec.many read >] -> List (Symbol "quote"::c)

let lexer =
  Lexer.make_lexer Lexer.scheme

let parse stream =
  Parsec.many read @@ lexer stream

let parse_string string =
  parse @@ Stream.of_string string
