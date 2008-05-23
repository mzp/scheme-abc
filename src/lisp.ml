open Base
open Lparser

let rec make_ast =
  function
      String s -> Ast.String s 
    | Int n -> Ast.Int n
    | Symbol name -> Ast.Var name
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
	  | Symbol "let"::List vars::body ->
	      let inits = 
		List.map (fun (List [Symbol n;init]) -> (n,make_ast init)) vars in
	      let body =
		List.map make_ast body in
	      Ast.Let (inits,Ast.Block body)
	  | Symbol "begin"::body ->
	      Ast.Block (List.map make_ast body)
	  | ((Symbol name)::args) ->
	      Ast.Call (name,List.map make_ast args)
	  | _ ->
	      failwith "make_ast" end

let compile stream = 
  List.map make_ast @@ Lparser.parse stream

let compile_string string =
  compile @@ Stream.of_string string
