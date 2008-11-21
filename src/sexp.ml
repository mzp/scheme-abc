(* lisp parser *)
open Base
type lisp = 
    Int    of int
  | String of string
  | Float  of float 
  | Bool   of bool 
  | Symbol of string 
  | List   of lisp list

type t =
    Int_    of int Node.t
  | String_ of string Node.t
  | Float_  of float Node.t
  | Bool_   of bool Node.t
  | Symbol_ of string Node.t
  | List_   of t list Node.t
    

let rec to_string =
  function
      Int_   node ->
	Node.to_string string_of_int node
    | String_ node ->
	Node.to_string (Printf.sprintf "\"%s\"") node
    | Float_  node ->
	Node.to_string string_of_float node
    | Symbol_ node ->
	Node.to_string id node
    | Bool_   node ->
	Node.to_string (fun b -> if b then "#t" else "#f") node
    | List_   node ->
	let f xs = 
	  let s = String.concat " " @@ List.map to_string xs in
	    Printf.sprintf "(%s)" s in
	  Node.to_string f node

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

let rec read_node =
  parser
      [<'{Node.value = Genlex.String s} as node>] -> 
	String_ {node with Node.value = s}
    | [<'{Node.value = Genlex.Ident name} as node>] ->
	Symbol_ {node with Node.value = name}
    | [<'{Node.value = Genlex.Int n} as node >] -> 
	Int_    {node with Node.value = n}
    | [<'{Node.value = Genlex.Float x} as node>] -> 
	Float_  {node with Node.value = x}
    | [<'{Node.value = Genlex.Kwd "true"} as node>] -> 
	Bool_   {node with Node.value = true}
    | [<'{Node.value=Genlex.Kwd "false"} as node >] -> 
	Bool_   {node with Node.value = false}
    | [<'{Node.value=Genlex.Kwd "("} as node; 
	c = Parsec.many read_node;
	'{Node.value = Genlex.Kwd ")"} >] -> 
	List_   {node with Node.value = c}
    | [<'{Node.value=Genlex.Kwd "["} as node;
	c = Parsec.many read_node;
	'{Node.value=Genlex.Kwd "]"} >] -> 
	List_   {node with Node.value = c}
    | [<'{Node.value=Genlex.Kwd "'"} as node; c = Parsec.many read_node >] -> 
	let quote =
	  Symbol_ {node with Node.value= "quote"} in
	  List_ {node with Node.value = quote::c}

let of_stream stream =
  let lexer =
    Lexer.lexer Lexer.scheme' in
    Parsec.many read_node @@ lexer stream

let of_string string =
  of_stream @@ Node.of_string string
