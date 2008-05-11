open Base
open Lparser

let rec make_ast =
  function
      String s -> 
	Ast.String s 
    | Int n ->
	Ast.Int n
    | List ((Symbol name)::args) ->
	Ast.Call (name,List.map make_ast args)    | _ ->
	failwith "make_ast"

let compile stream = 
  List.map make_ast @@ Lparser.parse stream

let compile_string string =
  compile @@ Stream.of_string string
