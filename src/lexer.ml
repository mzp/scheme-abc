open Base
open Parsec

let kwd    =
  Node.lift (fun x -> Genlex.Kwd x)

let ident  =
  Node.lift (fun x -> Genlex.Ident x)

let string =
  Node.lift (fun x -> Genlex.String x)

let int    =
  Node.lift (fun x -> Genlex.Int x)

let float  =
  Node.lift (fun x -> Genlex.Float x)

let implode =
  Node.concat ExtString.String.implode


let parse_keyword keywords stream = 
  let parse = 
    HList.fold_left1 (<|>) @@ List.map CharS.string keywords in
    Genlex.Kwd (ExtString.String.implode @@ parse stream)

let p_keyword keywords stream = 
  let parse = 
    HList.fold_left1 (<|>) @@ List.map NodeS.string keywords in
    kwd (Node.lift ExtString.String.implode @@ parse stream)

let parse_comment start stream =
  ignore @@ Parsec.string start stream;
  ignore @@ until '\n' stream;
  Stream.junk stream

let p_comment start stream =
  ignore @@ NodeS.string start stream;
  ignore @@ untilBy (fun {Node.value=c} -> c = '\n') stream;
  Stream.junk stream

let parse_space =
  ignore $ one_of " \t\n\r"

let space =
  ignore $ NodeS.one_of " \t\n\r"

let parse_ident head tail =
  let head = 
    alpha <|> one_of head in
  let tail = 
    head <|> digit <|> one_of tail in
    parser 
	[< x = head; xs = many tail>] ->
	  Genlex.Ident (ExtString.String.implode @@ x::xs)

let p_ident head tail =
  let head = 
    NodeS.alpha <|> NodeS.one_of head in
  let tail = 
    head <|> NodeS.digit <|> NodeS.one_of tail in
    parser 
	[< x = head; xs = many tail>] ->
	  ident @@ implode @@ x::xs

let p_char = 
  let escaped = 
    List.map (fun c -> ((Char.escaped c).[1],c)) ['\n'; '\t'] in
  parser [<'c; stream >] ->
    if c.Node.value = '\\' then
      let {Node.value = x} as node = 
	Stream.next stream in
	try
	  {node with
	     Node.value = List.assoc x escaped}
	with Not_found ->
	  node
    else
      c

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

let in_string stream =
  match Stream.peek stream with
      Some {Node.value = '"'} ->
	fail ()
    | _ ->
	p_char stream

let p_string delim =
  parser
      [<n = node delim; xs = many in_string; _ = node delim>] -> 
	{n with
	   Node.value = Genlex.String (ExtString.String.implode @@ List.map Node.value xs)}
    | [<>] ->
	fail ()

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

let p_int stream =
  let sign = 
    option (NodeS.one_of "-+") stream in
    match stream with parser
	[<e = many1 NodeS.digit >] ->
	  let n =
	    Node.lift int_of_string @@ implode e in
	    match sign with
		Some {Node.value = '-'} ->
		  int @@ Node.lift (~-) n
	      | _ ->
		  int n

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

let p_number stream =
  match stream with parser
      [<{Node.value=Genlex.Int x} as node = p_int>] ->
	begin match stream with parser
	    [<'{Node.value='.'}; y = many NodeS.digit >] ->
	      let v =
		Printf.sprintf "%d.%s" x (ExtString.String.implode @@ List.map Node.value y) in
		float {node with Node.value = float_of_string v}
	  | [<>] ->
	      node
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
  match (Parsec.CharS.string "#t" <|> Parsec.CharS.string "#f") stream with
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
    Node.of_string s in
  let result =
    try
      Some (f stream) 
    with _ ->
      None in
    Stream.iter (fun {Node.value=v} -> print_char v) stream;
    result,stream

type t = Genlex.token Node.t
type 'a lex = char Node.t Stream.t -> 'a

type laungage = { string_:  t lex;
		  number_:  t lex;
		  keyword_: t lex;
		  ident_:   t lex;
		  comment_: unit lex;
		  bool_:    t lex
		}

let lexer {string_ = string;
	   number_ = number;
	   keyword_= keyword;
	   ident_  = ident;
	   comment_= comment;
	   bool_   = bool;
	  } stream = 
  let token =
    string <|> keyword <|> try_ number <|> ident <|> bool in
  Stream.from (fun _ -> 
		 try
		   ignore @@ many (space <|> comment) stream;
		   Some (token stream)
		 with Stream.Failure -> None)

(* config *)
let scheme_bool stream =
  match (Parsec.CharS.string "#t" <|> Parsec.CharS.string "#f") stream with
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

let p_bool stream =
  match (Parsec.NodeS.string "#t" <|> Parsec.NodeS.string "#f") stream with
      {Node.value = ['#';'t']} as node -> kwd {node with Node.value="true"}
    | {Node.value = ['#';'f']} as node -> kwd {node with Node.value="false"}
    | _ -> failwith "must not happen: parse_bool"

let scheme' = {
  string_  = p_string '"';
  number_  = p_number;
  keyword_ = p_keyword ["(";")";"[";"]";"'"];
  ident_   = p_ident "!$%&*/:<=>?^_~+-*." "+-.@";
  comment_ = p_comment ";";
  bool_    = p_bool
}
