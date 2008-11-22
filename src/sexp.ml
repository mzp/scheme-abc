(* lisp parser *)
open Base
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

let rec read_node =
  parser
      [<'{Node.value = Genlex.String s} as node>] -> 
	String {node with Node.value = s}
    | [<'{Node.value = Genlex.Ident name} as node>] ->
	Symbol {node with Node.value = name}
    | [<'{Node.value = Genlex.Int n} as node >] -> 
	Int    {node with Node.value = n}
    | [<'{Node.value = Genlex.Float x} as node>] -> 
	Float  {node with Node.value = x}
    | [<'{Node.value = Genlex.Kwd "true"} as node>] -> 
	Bool   {node with Node.value = true}
    | [<'{Node.value=Genlex.Kwd "false"} as node >] -> 
	Bool   {node with Node.value = false}
    | [<'{Node.value=Genlex.Kwd "("} as node; 
	c = Parsec.many read_node;
	'{Node.value = Genlex.Kwd ")"} >] -> 
	List   {node with Node.value = c}
    | [<'{Node.value=Genlex.Kwd "["} as node;
	c = Parsec.many read_node;
	'{Node.value=Genlex.Kwd "]"} >] -> 
	List   {node with Node.value = c}
    | [<'{Node.value=Genlex.Kwd "'"} as node; c = Parsec.many read_node >] -> 
	let quote =
	  Symbol {node with Node.value= "quote"} in
	  List {node with Node.value = quote::c}

let of_stream stream =
  let lexer =
    Lexer.lexer Lexer.scheme in
    Parsec.many read_node @@ lexer stream

let of_string string =
  of_stream @@ Node.of_string string
