open Base
open Sexp
open ClosTrans
open Parsec

exception Syntax_error

let symbol = function
    Symbol n -> n
  | _ -> failwith "expected symbol"

let qname symbol =
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

let symbol v =
  Parsec.char (Symbol v)




::List vars::body | Symbol "letrec"::List vars::body ->
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
	      Ast.Lambda (List.map symbol args,Ast.Block body')
	  | Symbol "new"::Symbol name::args ->
	      Ast.New (qname name,List.map make_expr args)
	  | [Symbol "."; obj; List (Symbol name::args)] ->
	      Ast.Invoke (make_expr obj,name,List.map make_expr args)
	  | [Symbol "slot-ref";obj;Symbol name] ->
	      Ast.SlotRef (make_expr obj,name)
	  | [Symbol "slot-set!";obj;Symbol name;value] ->
	      Ast.SlotSet (make_expr obj,name,make_expr value)
	  | _ ->
	      Ast.Call (List.map make_expr xs)


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
	  | Symbol "cond"::conds ->
	      List.fold_right 
		(fun expr sub ->
		   match expr with
		       List ((Symbol "else")::body) ->
			 Ast.Block (List.map make_expr body)
		     | List (cond::body) ->
			 Ast.If (make_expr cond,Ast.Block (List.map make_expr body),sub)
		     | _ ->
			 failwith "syntax error") conds (Ast.Block [])
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
	      Ast.Lambda (List.map symbol args,Ast.Block body')
	  | Symbol "new"::Symbol name::args ->
	      Ast.New (qname name,List.map make_expr args)
	  | [Symbol "."; obj; List (Symbol name::args)] ->
	      Ast.Invoke (make_expr obj,name,List.map make_expr args)
	  | [Symbol "slot-ref";obj;Symbol name] ->
	      Ast.SlotRef (make_expr obj,name)
	  | [Symbol "slot-set!";obj;Symbol name;value] ->
	      Ast.SlotSet (make_expr obj,name,make_expr value)
	  | _ ->
	      Ast.Call (List.map make_expr xs)
	end

let make_stmt =
  function
      List (Symbol "define"::Symbol name::body) ->
	(* (define x 42) *)
	let body'=
	  List.map make_expr body in
	  ClosTrans.Plain (Ast.Define (name,Ast.Block body'))
    | List (Symbol "define"::List (Symbol name::args)::body) ->
	(* (define (x y) 42) *)
	let args' =
	  List.map symbol args in
	let body'=
	  Ast.Block (List.map make_expr body) in
	let f = 
	  Ast.Lambda (args',body') in
	  Plain (Ast.Define (name,f))
    | List [Symbol "define-class"; Symbol name; List (Symbol super::_); List attr] ->
	DefineClass (name,qname super,List.map symbol attr)
    | List (Symbol "define-method"::Symbol f::List (List [Symbol self;Symbol klass]::args)::body) ->
	DefineMethod (f,(self,klass),List.map symbol args,
		      Ast.Block (List.map make_expr body))
    | expr ->
	Plain (Ast.Expr (make_expr expr))

let compile stream = 
  List.map make_stmt @@ Sexp.parse stream

let compile_string string =
  compile @@ Stream.of_string string
