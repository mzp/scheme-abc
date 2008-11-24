open Base
open Sexp
open Parsec
open ClosTrans

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
      Some (List xs) ->
	let c = 
	  f @@ Stream.of_list xs.Node.value in
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

let keyword kwd stream =
  match Stream.peek stream with
      Some (Symbol {Node.value = v}) when kwd = v->
	Stream.next stream;
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
      [< _ = keyword "else"; body = block>] ->
	`Else body
    | [< cond = expr; body = block>] ->
	`Cond (cond,body)
and p_list =
  parser
      [< _ = keyword "if"; t = expr; c = expr; a = expr >] ->
	Ast.If (t,c,a)
    | [< _ =keyword "cond"; body = Parsec.many @@ list cond_clause >] ->
	List.fold_right 
	  (fun clause sub ->
	     match clause with
		 `Else body ->
		   body
	       | `Cond (cond,body) ->
		   Ast.If (cond,body,sub))
	  body (Ast.Block [])
    | [< _ = keyword "let"; vars = list @@ Parsec.many @@ list vars; 
	 body = Parsec.many expr>] ->
	Ast.Let (vars,Ast.Block body)
    | [< _ = keyword "letrec"; vars = list @@ Parsec.many @@ list vars; 
	 body = block>] ->
	Ast.LetRec (vars,body)
    | [< _ = keyword "begin"; body = block >] ->
	body
    | [< _ = keyword "lambda"; args = list @@ Parsec.many symbol; body = block >] ->
	Ast.Lambda (args,body)
    | [< _ = keyword "new"; name = symbol; args = Parsec.many expr >] ->
	Ast.New (qname name,args)
    | [< _ = keyword "."; obj = expr; (name,args) = list @@ one_list symbol expr >] ->
	Ast.Invoke (obj,name,args)
    | [< _ = keyword "slot-ref"; obj = expr; name = symbol >] ->
	Ast.SlotRef (obj,name)
    | [< _ = keyword "slot-set!";obj = expr; 
	 name = symbol; value = expr>] ->
	Ast.SlotSet (obj,name,value)
    | [< xs = Parsec.many expr >]  ->
	Ast.Call xs

let define_value =
  parser
      [< _ = keyword "define"; name = symbol; body = Parsec.many expr >] ->
	ClosTrans.Plain (Ast.Define (name,Ast.Block body))

let define_func =
  parser
      [< _ = keyword "define"; (name,args) = list @@ one_list symbol symbol; body = block >] ->
	let f = 
	  Ast.Lambda (args,body) in
	  ClosTrans.Plain (Ast.Define (name,f))

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
	ClosTrans.DefineClass (name,qname super,attr)
    | [< _ = keyword "define-method";
	 f = symbol;
	 ((self,klass),args) = list @@ one_list (list @@ pair symbol symbol) symbol;
	 body = block >] ->
	ClosTrans.DefineMethod (f,(self,klass),args, body)

let stmt =
  parser
      [< s = list p_stmt >] ->
	s
    | [< x = expr >] ->
	ClosTrans.Plain (Ast.Expr x)

let loc s =
  function
      Int n ->
	{n with Node.value = s}
    | String n ->
	{n with Node.value = s}
    | Float n ->
	{n with Node.value = s}
    | Bool n ->
	{n with Node.value = s}
    | Symbol n ->
	{n with Node.value = s}
    | List n ->
	{n with Node.value = s}

let eof s =
  Node.empty s

let compile stream = 
  let stream' =
    Stream.of_list @@ Sexp.of_stream stream in
    many (syntax_error (stmt <?> "malformed syntax") (loc "")) stream'


let compile_string string =
  compile @@ Node.of_string string

