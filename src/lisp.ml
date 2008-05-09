open Base
open Lparser

let rec make_ast =
  function
      String s -> 
	Ast.String s 
    | List ((Symbol name)::args) ->
	Ast.Call (name,List.map make_ast args)
    | _ ->
	failwith "unexpected call"

let compile stream = 
  List.map make_ast @@ Lparser.read stream

let compile_string string =
  compile @@ Stream.of_string string

