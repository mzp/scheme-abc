type token = Genlex.token Type.Node.t
type 'a lexer = char Type.Node.t Stream.t -> 'a
type lang = {
  string : token lexer;
  number : token lexer;
  keyword : token lexer;
  ident : token lexer;
  comment : unit lexer;
  bool : token lexer;
}

val scheme : lang
val lexer : lang -> char Type.Node.t Stream.t -> token Stream.t

