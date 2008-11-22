open Base
open Node
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

let rec implode =
  function
      [] ->
	Node.empty ""
    | [x] ->
	Node.lift string_of_char x
    | {Node.value=x; start_pos=a}::xs ->
	let {Node.value = ys} as node =
	  implode xs in
	  {node with
	     Node.value = (string_of_char x)^ys;
	     start_pos  = a}

let parse_keyword keywords stream = 
  let parse = 
    HList.fold_left1 (<|>) @@ List.map NodeS.string keywords in
    kwd (Node.lift ExtString.String.implode @@ parse stream)

let parse_comment start stream =
  ignore @@ NodeS.string start stream;
  ignore @@ untilBy (fun {Node.value=c} -> c = '\n') stream;
  Stream.junk stream

let parse_space =
  ignore $ NodeS.one_of " \t\n\r"

let parse_ident head tail =
  let head = 
    NodeS.alpha <|> NodeS.one_of head in
  let tail = 
    head <|> NodeS.digit <|> NodeS.one_of tail in
    parser 
	[< x = head; xs = many tail>] ->
	  ident @@ implode @@ x::xs

let parse_char =
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

let in_string stream =
  match Stream.peek stream with
      Some {Node.value = '"'} ->
	fail ()
    | _ ->
	parse_char stream

let parse_string delim =
  parser
      [<n = node delim; {value = xs} = implode $ many in_string; 
	{end_pos=e} = node delim>] -> 
	{n with
	   value   = Genlex.String xs;
	   end_pos = e}
    | [<>] ->
	fail ()

let parse_int stream =
  let sign = 
    option (NodeS.one_of "-+") stream in
    match stream with parser
	[<e = implode $ many1 NodeS.digit >] ->
	  let n =
	    Node.lift int_of_string e in
	    match sign with
		Some {Node.value = '-'} ->
		  int @@ Node.lift (~-) n
	      | _ ->
		  int n

let parse_number stream =
  match stream with parser
      [<{Node.value=Genlex.Int x} as node = parse_int>] ->
	begin match stream with parser
	    [<'{Node.value='.'}; {Node.value=y; end_pos=pos} = 
		implode $ many NodeS.digit >] ->
	      let v =
		Printf.sprintf "%d.%s" x y in
		float {node with 
			 Node.value = float_of_string v;
			 end_pos    = pos}
	  | [<>] ->
	      node
	end
    | [<>] ->
	fail ()

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

type token = Genlex.token Node.t
type 'a lexer = char Node.t Stream.t -> 'a

type lang = { string:  token lexer;
	      number:  token lexer;
	      keyword: token lexer;
	      ident:   token lexer;
	      comment: unit  lexer;
	      bool:    token lexer;
	    }

let lexer {string = string;
	   number = number;
	   keyword= keyword;
	   ident  = ident;
	   comment= comment;
	   bool   = bool;
	  } stream = 
  let token =
    string <|> keyword <|> try_ number <|> ident <|> bool in
  Stream.from (fun _ -> 
		 try
		   ignore @@ many (parse_space <|> comment) stream;
		   Some (token stream)
		 with Stream.Failure -> None)

let parse_bool stream =
  match (Parsec.NodeS.string "#t" <|> Parsec.NodeS.string "#f") stream with
      {Node.value = ['#';'t']} as node -> kwd {node with Node.value="true"}
    | {Node.value = ['#';'f']} as node -> kwd {node with Node.value="false"}
    | _ -> failwith "must not happen: parse_bool"

let scheme = {
  string  = parse_string '"';
  number  = parse_number;
  keyword = parse_keyword ["(";")";"[";"]";"'"];
  ident   = parse_ident "!$%&*/:<=>?^_~+-*." "+-.@";
  comment = parse_comment ";";
  bool    = parse_bool
}
