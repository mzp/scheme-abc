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

let kwd s =
  node (Genlex.Kwd s)

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
    | [< node = kwd "true" >] ->
	Bool   {node with value = true}
    | [< node = kwd "false" >] ->
	Bool   {node with value = false}
    | [< e = parse_list <?> "unbalanced list" >] ->
	e
    | [< node = kwd "'"; c = read >] ->
	let quote =
	  Symbol {node with value= "quote"} in
	  List {node with value = [quote;c]}
and parse_list =
  parser
      [< node = kwd "(";
	 c = Parsec.many read;
	 {end_pos = pos} = kwd ")" >] ->
	List   {node with value = c; end_pos = pos}
    | [< node = kwd "[";
	c = Parsec.many read;
	{end_pos = pos} = kwd "]" >] ->
	List   {node with value = c; end_pos = pos}

let of_stream stream =
  Parsec.many (Parsec.syntax_error read id) stream
