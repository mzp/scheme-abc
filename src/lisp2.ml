open Base
open Sexp
open Parsec
open ClosTrans

exception Syntax_error of string

let qname ({Node.value = sym} as node) =
  try
    let n =
      String.rindex sym '.' in
    let ns =
      String.sub sym 0 n in
    let name =
      String.sub sym (n+1) ((String.length sym) - n - 1) in
      {node with Node.value = (ns,name)}
  with Not_found ->
    {node with Node.value = ("",sym)}

let list f stream =
  match Stream.peek stream with
      Some (List_ xs) ->
	let c = 
	  f @@ Stream.of_list xs.Node.value in
	  Stream.junk stream;
	  c
    | _ ->
	Parsec.fail ()

let symbol stream =
  match Stream.peek stream with
      Some (Symbol_ s) ->
	Stream.junk stream;
	s
    | _ ->
	Parsec.fail ()

let keyword kwd stream =
  match Stream.peek stream with
      Some (Symbol_ {Node.value = v}) when kwd = v->
	Stream.next stream;
    | _ ->
	Parsec.fail ()

let one_list hd tl =
  parser 
      [< x = hd; y = Parsec.many tl>] ->
	(x,y)



let rec expr = 
  parser
      [<' Int_ n       >] ->
	Ast2.Int n
    | [<' String_ s    >] ->
	Ast2.String s
    | [<' Bool_ b      >] -> 
	Ast2.Bool b
    | [<' Float_ v     >] -> 
	Ast2.Float v
    | [<' Symbol_ name >] -> 
	Ast2.Var name
    | [< e = list p_list >] ->
	e
and vars =
  parser
      [<' Symbol_ var; init = expr >] ->
	(var,init)
and block =
  parser
      [< e = Parsec.many expr >] ->
	Ast2.Block e
and cond_clause =
  parser
      [< _ = keyword "else"; body = block>] ->
	`Else body
    | [< cond = expr; body = block>] ->
	`Cond (cond,body)
and p_list =
  parser
      [< _ = keyword "if"; t = expr; c = expr; a = expr >] ->
	Ast2.If (t,c,a)
    | [< _ =keyword "cond"; body = Parsec.many @@ list cond_clause >] ->
	List.fold_right 
	  (fun clause sub ->
	     match clause with
		 `Else body ->
		   body
	       | `Cond (cond,body) ->
		   Ast2.If (cond,body,sub))
	  body (Ast2.Block [])
    | [< _ = keyword "let"; vars = list @@ Parsec.many @@ list vars; 
	 body = Parsec.many expr>] ->
	Ast2.Let (vars,Ast2.Block body)
    | [< _ = keyword "letrec"; vars = list @@ Parsec.many @@ list vars; 
	 body = block>] ->
	Ast2.LetRec (vars,body)
    | [< _ = keyword "begin"; body = block >] ->
	body
    | [< _ = keyword "lambda"; args = list @@ Parsec.many symbol; body = block >] ->
	Ast2.Lambda (args,body)
    | [< _ = keyword "new"; name = symbol; args = Parsec.many expr >] ->
	Ast2.New (qname name,args)
    | [< _ = keyword "."; obj = expr; (name,args) = list @@ one_list symbol expr >] ->
	Ast2.Invoke (obj,name,args)
    | [< _ = keyword "slot-ref"; obj = expr; name = symbol >] ->
	Ast2.SlotRef (obj,name)
    | [< _ = keyword "slot-set!";obj = expr; 
	 name = symbol; value = expr>] ->
	Ast2.SlotSet (obj,name,value)
    | [< xs = Parsec.many expr >]  ->
	Ast2.Call xs

let define_value =
  parser
      [< _ = keyword "define"; name = symbol; body = Parsec.many expr >] ->
	ClosTrans2.Plain (Ast2.Define (name,Ast2.Block body))

let define_func =
  parser
      [< _ = keyword "define"; (name,args) = list @@ one_list symbol symbol; body = block >] ->
	let f = 
	  Ast2.Lambda (args,body) in
	  ClosTrans2.Plain (Ast2.Define (name,f))

let define =
  (try_ define_value) <|> define_func

let pair car cdr =
  parser [< x = car; y = cdr >] ->
    (x,y)

let p_stmt =
  parser
      [< def = define >] ->
	def
    | [< _ = keyword "define-class"; 
	 name = symbol;
	 (super,_)= list @@ one_list symbol symbol; 
	 attr = list @@ many symbol >] ->
	ClosTrans2.DefineClass (name,qname super,attr)
    | [< _ = keyword "define-method";
	 f = symbol;
	 ((self,klass),args) = list @@ one_list (list @@ pair symbol symbol) symbol;
	 body = block >] ->
	ClosTrans2.DefineMethod (f,(self,klass),args, body)

let stmt =
  parser
      [< s = list p_stmt >] ->
	s
    | [< xs = many1 expr >] ->
	match xs with
	    [x] ->
	      ClosTrans2.Plain (Ast2.Expr x)
	  | xs ->
	      ClosTrans2.Plain (Ast2.Expr (Ast2.Block xs))

let compile stream = 
  try
    many stmt @@ Stream.of_list @@ Sexp.of_stream stream
  with
      Stream.Error s ->
	raise (Syntax_error s)

let compile_string string =
  compile @@ Node.of_string string

