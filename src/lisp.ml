open Base
open Sexp

let ensure_symbol = function
    Symbol n -> n
  | _ -> failwith "expected symbol"

let split_ns symbol =
  try
    let n =
      String.rindex symbol '.' in
    let ns =
      String.sub symbol 0 n in
    let name =
      String.sub symbol (n+1) ((String.length symbol) - n - 1) in
      ns,name
  with Not_found ->
    "",symbol

let rec make_expr =
  function
      String s -> Ast.String s 
    | Bool b -> Ast.Bool b
    | Float v -> Ast.Float v
    | Int n -> Ast.Int n
    | Symbol name -> Ast.Var name
    | List xs -> 
	begin match xs with
	    [Symbol "if";t;c;a] ->
	      Ast.If (make_expr t,make_expr c,make_expr a)
	  | Symbol "let"::List vars::body | Symbol "letrec"::List vars::body ->
	      let inits = 
		List.map 
		  (function 
		       (List [Symbol n;init]) -> (n,make_expr init) 
		     | _ -> failwith "") 
		  vars in
	      let body' =
		List.map make_expr body in
		if List.hd xs = Symbol "let" then
		  Ast.Let (inits,Ast.Block body')
		else
		  Ast.LetRec (inits,Ast.Block body')
	  | Symbol "begin"::body ->
	      Ast.Block (List.map make_expr body)
	  | Symbol "lambda"::List args::body ->
	      let body' =
		List.map make_expr body in
	      Ast.Lambda (List.map ensure_symbol args,Ast.Block body')
	  | _ ->
	      Ast.Call (List.map make_expr xs)
	end


let make_stmt =
  function
      List (Symbol "define"::Symbol name::body) ->
	(* (define x 42) *)
	let body'=
	  List.map make_expr body in
	Ast.Define (name,Ast.Block body')
    | List (Symbol "define"::List (Symbol name::args)::body) ->
	(* (define (x y) 42) *)
	let args' =
	  List.map ensure_symbol args in
	let body'=
	  Ast.Block (List.map make_expr body) in
	let f = 
	  Ast.Lambda (args',body') in
	Ast.Define (name,f)
    | List (Symbol "define-class"::Symbol name::Symbol sname::body) ->
	(* "(define-class Foo Object ((init x) x))" *)
	let body' =
	  List.map (function List ((List x)::xs) -> 
		      begin match List.map ensure_symbol x with
			  name::args ->
			    let xs' = 
			      List.map make_expr xs in
			      (name,args,Ast.Block xs')
			| _ ->
			    failwith "syntax error"
		      end
		      | _ -> failwith "syntax error") body in
	  Ast.Class (name,split_ns sname,body')	  
    | expr ->
	Ast.Expr (make_expr expr)
  
let compile stream = 
  List.map make_stmt @@ Sexp.parse stream

let compile_string string =
  compile @@ Stream.of_string string
