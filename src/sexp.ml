(* lisp parser *)
open Base
open Node
open Parsec

type t = 
    Int    of int Node.t
  | String of string Node.t
  | Float  of float Node.t
  | Bool   of bool Node.t
  | Symbol of string Node.t
  | List   of t list Node.t

let rec to_string =
  function
      Int   node ->
	Node.to_string string_of_int node
    | String node ->
	Node.to_string (Printf.sprintf "\"%s\"") node
    | Float  node ->
	Node.to_string string_of_float node
    | Symbol node ->
	Node.to_string id node
    | Bool   node ->
	Node.to_string (fun b -> if b then "#t" else "#f") node
    | List   node ->
	let f xs = 
	  let s = String.concat " " @@ List.map to_string xs in
	    Printf.sprintf "(%s)" s in
	  Node.to_string f node

let rec read =
  parser
      [<'{value = Genlex.String s} as node>] -> 
	String {node with value = s}
    | [<'{value = Genlex.Ident name} as node>] ->
	Symbol {node with value = name}
    | [<'{value = Genlex.Int n} as node >] -> 
	Int    {node with value = n}
    | [<'{value = Genlex.Float x} as node>] -> 
	Float  {node with value = x}
    | [<'{value = Genlex.Kwd "true"} as node>] -> 
	Bool   {node with value = true}
    | [<'{value=Genlex.Kwd "false"} as node >] -> 
	Bool   {node with value = false}
    | [< e = parse_list <?> "unbalanced list" >] ->
	e
    | [<'{value=Genlex.Kwd "'"} as node; c = read >] -> 
	let quote =
	  Symbol {node with value= "quote"} in
	  List {node with value = [quote;c]}
and parse_list =
  parser
      [<'{value=Genlex.Kwd "("} as node; 
	c = Parsec.many read;
	'{value = Genlex.Kwd ")"; end_pos = pos} >] -> 
	List   {node with value = c; end_pos = pos}
    | [<'{value=Genlex.Kwd "["} as node;
	c = Parsec.many read;
	'{value=Genlex.Kwd "]"; end_pos = pos} >] -> 
	List   {node with value = c; end_pos = pos}

let of_stream stream =
  Parsec.many (Parsec.syntax_error read id) @@ 
    Lexer.lexer Lexer.scheme stream

let of_string string =
  of_stream @@ Node.of_string string
