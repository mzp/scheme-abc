open Base
open Sexp
open Parsec
open ClosTrans

exception Syntax_error of string

let qname symbol_sure =
  try
    let n =
      String.rindex symbol_sure '.' in
    let ns =
      String.sub symbol_sure 0 n in
    let name =
      String.sub symbol_sure (n+1) ((String.length symbol_sure) - n - 1) in
      ns,name
  with Not_found ->
    "",symbol_sure

let list f stream =
  match Stream.peek stream with
      Some (List xs) ->
	let c = 
	  f @@ Stream.of_list xs in
	  Stream.junk stream;
	  c
    | _ ->
	Parsec.fail ()

let symbol stream =
  match Stream.peek stream with
      Some (Symbol s) ->
	Stream.junk stream;
	s
    | _ ->
	Parsec.fail ()

let one_list hd tl =
  parser 
      [< x = hd; y = Parsec.many tl>] ->
	(x,y)

let rec expr = 
  parser
      [<' Int n       >] ->
	Ast.Int n
    | [<' String s    >] ->
	Ast.String s
    | [<' Bool b      >] -> 
	Ast.Bool b
    | [<' Float v     >] -> 
	Ast.Float v
    | [<' Symbol name >] -> 
	Ast.Var name
    | [< e = list p_list >] ->
	e
and vars =
  parser
      [<' Symbol var; init = expr >] ->
	(var,init)
and block =
  parser
      [< e = Parsec.many expr >] ->
	Ast.Block e
and cond_clause =
  parser
      [< 'Symbol "else"; body = block>] ->
	`Else body
    | [< cond = expr; body = block>] ->
	`Cond (cond,body)
and p_list =
  parser
      [<' Symbol "if"; t = expr; c = expr; a = expr >] ->
	Ast.If (t,c,a)
    | [< 'Symbol "cond"; body = Parsec.many @@ list cond_clause >] ->
	List.fold_right 
	  (fun clause sub ->
	     match clause with
		 `Else body ->
		   body
	       | `Cond (cond,body) ->
		   Ast.If (cond,body,sub))
	  body (Ast.Block [])
    | [<' Symbol "let"; vars = list @@ Parsec.many @@ list vars; body = Parsec.many expr>] ->
	Ast.Let (vars,Ast.Block body)
    | [<' Symbol "letrec"; vars = list @@ Parsec.many @@ list vars; body = block>] ->
	Ast.LetRec (vars,body)
    | [<' Symbol "begin"; body = block >] ->
	body
    | [<' Symbol "lambda"; args = list @@ Parsec.many symbol; body = block >] ->
	Ast.Lambda (args,body)
    | [<' Symbol "new"; name = symbol; args = Parsec.many expr >] ->
	Ast.New (qname name,args)
    | [<' Symbol "."; obj = expr; (name,args) = list @@ one_list symbol expr >] ->
	Ast.Invoke (obj,name,args)
    | [<' Symbol "slot-ref"; obj = expr; name = symbol >] ->
	Ast.SlotRef (obj,name)
    | [<' Symbol "slot-set!";obj = expr; name = symbol; value = expr>] ->
	Ast.SlotSet (obj,name,value)
    | [< xs = Parsec.many expr >]  ->
	Ast.Call xs

let define_value =
  parser
      [< 'Symbol "define"; name = symbol; body = Parsec.many expr >] ->
	ClosTrans.Plain (Ast.Define (name,Ast.Block body))

let define_func =
  parser
      [< 'Symbol "define"; (name,args) = list @@ one_list symbol symbol; body = block >] ->
	let f = 
	  Ast.Lambda (args,body) in
	  Plain (Ast.Define (name,f))

let define =
  (try_ define_value) <|> define_func

let pair car cdr =
  parser [< x = car; y = cdr >] ->
    (x,y)

let p_stmt =
  parser
      [< def = define >] ->
	def
    | [< 'Symbol "define-class"; 
	 name = symbol;
	 (super,_)= list @@ one_list symbol symbol; 
	 attr = list @@ many symbol >] ->
	DefineClass (name,qname super,attr)
    | [< 'Symbol "define-method";
	 f = symbol;
	 ((self,klass),args) = list @@ one_list (list @@ pair symbol symbol) symbol;
	 body = block >] ->
	DefineMethod (f,(self,klass),args, body)

let stmt =
  parser
      [< s = list p_stmt >] ->
	s
    | [< xs = many1 expr >] ->
	match xs with
	    [x] ->
	      Plain (Ast.Expr x)
	  | xs ->
	      Plain (Ast.Expr (Ast.Block xs))

let compile stream = 
  try
    many stmt @@ Stream.of_list @@ Sexp.parse stream
  with
      Stream.Error s ->
	raise (Syntax_error s)

let compile_string string =
  compile @@ Stream.of_string string

