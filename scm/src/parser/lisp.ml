open Base
open Sexp
open Parsec
open Node

let dot =
  Str.regexp "\\."
let qname ({Node.value = sym} as node) =
  match List.rev @@ Str.split_delim dot sym with
      [] ->
	failwith "must not happen"
    | [name] ->
	{node with Node.value = ([],name)}
    | ""::name::ns ->
	{node with Node.value = (List.rev ns,name^".")}
    | name::ns ->
	{node with Node.value = (List.rev ns,name)}

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

let variable_arguments = [
  "+";"-";"*";
  "+.";"-.";"*.";"/";
]

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
    | [< _ = kwd "array"; xs = Parsec.many expr >] ->
	`Array xs
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
    | [< _ = kwd "list"; xs = Parsec.many expr >] ->
	let cons x y =
	  `Call [`Var (Node.ghost ([],"cons")); x; y] in
	let nil =
	  `Var (Node.ghost ([],"nil")) in
	  List.fold_right cons xs nil
    | [< Symbol op = HList.fold_left1 (<|>) @@ List.map kwd variable_arguments;
	 args      = Parsec.many expr >]  ->
	let op' =
	  `Var (qname op) in
	  HList.fold_left1 (fun x y -> `Call [op'; x; y]) args
    | [< xs = Parsec.many1 expr >]  ->
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

let is_valid_module xs =
  if  xs = "" then
    false
  else
    match xs.[0] with
	'A' .. 'Z' ->
	  true
      | _ ->
	  false

(*
type 'expr method_ = {
  method_name : [`Public of sname | `Static of sname];
  args : sname list;
  body : 'expr;
}
*)
let p_method =
  parser
      [<_ = kwd "method"; name = symbol; args = list @@ many symbol; body = block>] ->
	{
	  Ast.method_name = `Public name;
	  args = args;
	  body = body
	}
    | [<_ = kwd "static"; name = symbol; args = list @@ many symbol; body = block>] ->
	{
	  Ast.method_name = `Static name;
	  args = args;
	  body = body
	}

let rec p_stmt =
  parser
      [< def = define >] ->
	def
    | [< _ = kwd "open"; module_name = symbol >] ->
	`Open (Node.lift (Str.split_delim dot) module_name)
    | [< _         = kwd "class";
	 name      = symbol;
	 (super,_) = list @@ one_list symbol symbol;
	 attrs     = list @@ many symbol;
	 methods   = many @@ list p_method>] ->
	`Class {Ast.class_name = name;
		super          = qname super;
		attrs          = attrs;
		methods        = methods}
    | [< _ = kwd "module"; name = symbol; exports = list @@ many symbol; stmts = many stmt>] ->
	if exports = [] then
	  (* exports nothing must not be happened. *)
	  `Module {Ast.module_name=name;
		   exports=`All;
		   stmts=stmts}
	else
	  `Module {Ast.module_name=name;
		   exports=`Only exports;
		   stmts=stmts}
and stmt =
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

let parse xs =
  let stream' =
    Stream.of_list xs in
  many (syntax_error (stmt <?> "malformed syntax") (loc "")) stream'
