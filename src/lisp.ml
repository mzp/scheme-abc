open Base
open Lparser

let rec make_ast =
  function
      String s -> Ast.String s 
    | Int n -> Ast.Int n
    | List xs -> 
	begin match xs with
	    [Symbol "+";l;r] ->
	      Ast.Add (make_ast l,make_ast r)
	  | [Symbol "-";l;r] ->
	      Ast.Sub (make_ast l,make_ast r)
	  | [Symbol "*";l;r] ->
	      Ast.Mul (make_ast l,make_ast r)
	  | [Symbol "/";l;r] ->
	      Ast.Div (make_ast l,make_ast r)
	  (* boolean operator *)
	  | [Symbol "=";l;r] ->
	      Ast.Eq (make_ast l,make_ast r)
	  | [Symbol ">";l;r] ->
	      Ast.Gt (make_ast l,make_ast r)
	  | [Symbol ">=";l;r] ->
	      Ast.Geq (make_ast l,make_ast r)
	  | [Symbol "<";l;r] ->
	      Ast.Lt (make_ast l,make_ast r)
	  | [Symbol "<=";l;r] ->
	      Ast.Leq (make_ast l,make_ast r)
	  | [Symbol "if";t;c;a] ->
	      Ast.If (make_ast t,make_ast c,make_ast a)
	  | ((Symbol name)::args) ->
	      Ast.Call (name,List.map make_ast args)
	  | _ ->
	      failwith "make_ast" end
    | _ ->
	failwith "make_ast"

let compile stream = 
  List.map make_ast @@ Lparser.parse stream

let compile_string string =
  compile @@ Stream.of_string string
