type 'a lexer = char Node.t Stream.t -> 'a
type lang = {
  string : Token.t lexer;
  number : Token.t lexer;
  keyword : Token.t lexer;
  ident : Token.t lexer;
  comment : unit lexer;
  bool : Token.t lexer;
}

val scheme : lang
val lexer : lang -> char Node.t Stream.t -> Token.t Stream.t

