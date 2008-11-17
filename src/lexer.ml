open Base
open Parsec

let parse_keyword keywords stream = 
  let parse = 
    HList.fold_left1 (<|>) @@ List.map string keywords in
    Genlex.Kwd (ExtString.String.implode @@ parse stream)

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
	  Genlex.Ident (ExtString.String.implode @@ x::xs)

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
	Genlex.String (ExtString.String.implode e)

let parse_int stream =
  let sign = 
    option (one_of "-+") stream in
    match stream with parser
	[<e = many1 digit >] ->
	  let n =
	    int_of_string @@ ExtString.String.implode e in
	    if sign = Some '-' then
	      Genlex.Int ~-n
	    else
	      Genlex.Int n

let parse_number stream =
  match stream with parser
      [<Genlex.Int x = parse_int>] ->
	begin match stream with parser
	    [<''.'; y = many digit >] ->
	      let v = 
		Printf.sprintf "%d.%s" x @@ ExtString.String.implode y in
		Genlex.Float (float_of_string v)
	  | [<>] ->
	      Genlex.Int x
	end
    | [<>] ->
	fail ()


type token = Genlex.token
type 'a lexer = char Stream.t -> 'a

type lang = { string:  token lexer;
	      number:  token lexer;
	      keyword: token lexer;
	      ident:   token lexer;
	      comment: unit  lexer;
	      bool:    token  lexer;
	    }

let make_lexer lang stream = 
  let token =
    lang.string <|> lang.keyword  <|> try_ lang.number <|> lang.ident <|> lang.bool in
  Stream.from (fun _ -> 
		 try
		   ignore @@ many (parse_space <|> lang.comment) stream;
		   Some (token stream)
		 with Stream.Failure -> None)

(* config *)
let scheme_bool stream =
  match (string "#t" <|> string "#f") stream with
      ['#';'t'] -> Genlex.Kwd "true"
    | ['#';'f'] -> Genlex.Kwd "false"
    | _ -> failwith "must not happen: parse_bool"

let scheme = {
  string  = parse_string '"';
  number  = parse_number;
  keyword = parse_keyword ["(";")";"[";"]";"'"];
  ident   = parse_ident "!$%&*/:<=>?^_~+-*." "+-.@" <|> (fun s-> Genlex.Ident (Char.escaped @@ one_of "/" s));
  comment = parse_comment ";";
  bool    = scheme_bool
}

let test f s =
  let stream =
    Stream.of_string s in
  let result =
    try
      Some (f stream) 
    with _ ->
      None in
    Stream.dump (print_char) stream;
    result
