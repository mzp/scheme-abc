(* obsolute *)
type token = Genlex.token
type 'a lexer = char Stream.t -> 'a
type lang = {
  string : token lexer;
  number : token lexer;
  keyword : token lexer;
  ident : token lexer;
  comment : unit lexer;
  bool : token lexer;
}
val make_lexer : lang -> char Stream.t -> token Stream.t

val scheme : lang

type t = Genlex.token Node.t
type 'a lex = char Node.t Stream.t -> 'a
type laungage = { string_:  t lex;
		  number_:  t lex;
		  keyword_: t lex;
		  ident_:   t lex;
		  comment_: unit lex;
		  bool_:    t lex
		}
val scheme' : laungage

val lexer : laungage -> char Node.t Stream.t -> token Node.t Stream.t
