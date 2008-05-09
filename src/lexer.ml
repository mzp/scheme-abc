open Base
open Parsec

let parse_string = parser 
    [<''"'; str=until '"'; ''"'>] ->
      Genlex.String (implode str)

let fold_right1 f xs = 
  List.fold_right f (List.tl xs) (List.hd xs) 

let fold_left1 f xs = 
  List.fold_left f (List.hd xs) (List.tl xs) 

let parse_keyword keywords stream = 
  let parse = 
    fold_left1 (<|>) @@ List.map string keywords in
    Genlex.Kwd (implode @@ parse stream)

let parse_comment start stream =
  ignore @@ string start stream;
  ignore @@ until '\n' stream;
  Stream.junk stream

let parse_space =
  ignore $ one_of " \t\n\r"

let parse_ident head tail =
  let head = 
    alpha <|> one_of head in
  let tail = 
    head <|> digit <|> one_of tail in
    parser 
	[< x = head; xs = many tail>] ->
	  Genlex.Ident (implode @@ x::xs)

let parse_char = 
  let escaped = 
    List.map (fun c -> ((Char.escaped c).[1],c)) ['\n'; '\t'] in
  parser [<'c; stream >] ->
    if c = '\\' then
      let x = 
	Stream.next stream in
	try
	  List.assoc x escaped
	with Not_found ->
	  x
    else
      c

let string_content stream = 
  match Stream.peek stream with
      Some '"' ->
	fail ()
    | _ ->
	parse_char stream

let parse_string delim =
  parser
      [<_ = char delim; e = many string_content; _ = char delim>] -> 
	Genlex.String (implode e)

let parse_int stream =
  let sign = 
    option (one_of "-+") stream in
    match stream with parser
	[<e = many1 digit >] ->
	  let n =
	    int_of_string @@ implode e in
	    if sign = Some '-' then
	      Genlex.Int ~-n
	    else
	      Genlex.Int n

type token = Genlex.token
type 'a lexer = char Stream.t -> 'a

type lang = { string: token lexer;
	      int: token lexer;
	      keyword: token lexer;
	      ident: token lexer;
	      comment: unit lexer
	    }

let make_lexer lang stream = 
  let token =
    lang.string <|> lang.keyword <|> lang.int <|> lang.ident in
  Stream.from (fun _ -> 
		 try
		   ignore @@ many (parse_space <|> lang.comment) stream;
		   Some (token stream)
		 with Stream.Failure -> None)

let scheme = {
  string = parse_string '"';
  int = parse_int;
  keyword = parse_keyword ["(";")"];
  ident = parse_ident "!$%&*/:<=>?^_~" "+-.@" <|> (fun s-> Genlex.Ident (Char.escaped @@ one_of "+-*/" s));
  comment = parse_comment ";";
}
