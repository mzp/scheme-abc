open Base
open Sexp
open Parsec
open Node

let qname ({Node.value = sym} as node) =
  try
    let n =
      String.rindex sym '.' in
    let ns =
      String.sub sym 0 n in
    let name =
      String.sub sym (n+1) ((String.length sym) - n - 1) in
      if name <> "" then
	{node with Node.value = (ns,name)}
      else
	{node with Node.value = ("",sym)}
  with Not_found ->
    {node with Node.value = ("",sym)}

let list f stream =
  match Stream.peek stream with
      Some (List {Node.value=xs}) ->
	let xs' =
	  Stream.of_list xs in
	let res =
	  f xs' in
	  Stream.junk stream;
	  if Stream.peek xs' <> None then
	    raise (Stream.Error "")
	  else
	    res
    | _ ->
	Parsec.fail ()

let symbol stream =
  match Stream.peek stream with
      Some (Symbol s) ->
	Stream.junk stream;
	s
    | _ ->
	Parsec.fail ()

let kwd kwd stream =
  match Stream.peek stream with
      Some (Symbol {Node.value = v}) when kwd = v->
	Stream.next stream;
    | _ ->
	Parsec.fail ()

let one_list hd tl =
  parser
      [< x = hd; y = Parsec.many tl>] ->
	(x,y)

let pair car cdr =
  parser [< x = car; y = cdr >] ->
    (x,y)

let rec expr =
  parser
      [<' Int n       >] ->
	`Int n
    | [<' String s    >] ->
	`String s
    | [<' Bool b      >] ->
	`Bool b
    | [<' Float v     >] ->
	`Float v
    | [<' Symbol name >] ->
	`Var (qname name)
    | [< e = list p_list >] ->
	e
and vars =
  parser
      [<' Symbol var; init = expr >] ->
	(var,init)
and block =
  parser
      [< e = Parsec.many expr >] ->
	`Block e
and cond_clause =
  parser
      [< _ = kwd "else"; body = block>] ->
	`Else body
    | [< cond = expr; body = block>] ->
	`Cond (cond,body)
and p_list =
  parser
      [< _ = kwd "if"; t = expr; c = expr; a = expr >] ->
	`If (t,c,a)
    | [< _ =kwd "cond"; body = Parsec.many @@ list cond_clause >] ->
	List.fold_right
	  (fun clause sub ->
	     match clause with
		 `Else body ->
		   body
	       | `Cond (cond,body) ->
		   `If (cond,body,sub))
	  body (`Block [])
    | [< _ = kwd "let"; vars = list @@ Parsec.many @@ list vars;
	 body = Parsec.many expr>] ->
	`Let (vars,`Block body)
    | [< _ = kwd "letrec"; vars = list @@ Parsec.many @@ list vars;
	 body = block>] ->
	`LetRec (vars,body)
    | [< _ = kwd "begin"; body = block >] ->
	body
    | [< _ = kwd "lambda"; args = list @@ Parsec.many symbol; body = block >] ->
	`Lambda (args,body)
    | [< _ = kwd "new"; name = symbol; args = Parsec.many expr >] ->
	`New (qname name,args)
    | [< _ = kwd "."; obj = expr; (name,args) = list @@ one_list symbol expr >] ->
	`Invoke (obj,name,args)
    | [< _ = kwd "slot-ref"; obj = expr; name = symbol >] ->
	`SlotRef (obj,name)
    | [< _ = kwd "slot-set!";obj = expr;
	 name = symbol; value = expr>] ->
	`SlotSet (obj,name,value)
    | [< xs = Parsec.many expr >]  ->
	`Call xs

let define_value =
  parser
      [< _ = kwd "define"; name = symbol; body = Parsec.many expr >] ->
	`Define (name,`Block body)

let define_func =
  parser
      [< _ = kwd "define"; (name,args) = list @@ one_list symbol symbol; body = block >] ->
	let f =
	  `Lambda (args,body) in
	  `Define (name,f)

let define =
  (try_ define_value) <|> define_func

let p_stmt : Sexp.t Stream.t -> ClosTrans.stmt =
  parser
      [< def = define >] ->
	def
    | [< _ = kwd "define-class";
	 name = symbol;
	 (super,_)= list @@ one_list symbol symbol;
	 attr = list @@ many symbol >] ->
	`DefineClass (name,qname super,attr)
    | [< _ = kwd "define-method";
	 f = symbol;
	 ((self,klass),args) = list @@ one_list (list @@ pair symbol symbol) symbol;
	 body = block >] ->
	`DefineMethod (f,(self,klass),args, body)
    | [< _ = kwd "external"; name = symbol >] ->
	`External (name)
    | [< _ = kwd "external-class"; name = symbol; methods = list @@ many symbol>] ->
	`ExternalClass (name,methods)

let stmt =
  parser
      [< s = list p_stmt >] ->
	s
    | [< x = expr >] ->
	(`Expr x)

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

