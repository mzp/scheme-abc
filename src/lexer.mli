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
