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

let compile = 
  List.map make_ast
