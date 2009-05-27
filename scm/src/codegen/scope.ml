open Base
type qname = (string list * string)

type env = {
  binding: qname list
}

let empty = {
  binding = []
}

let sname x =
  ([],x)

let add env x =
  {binding = x @ env.binding}

let expr_scope current env (expr : Ast.expr') : Ast.expr' =
  (Ast.fix_fold Ast.fold)
    begin fun env ->
      function
	  `Lambda (args,_) ->
	    args
	    +> List.map (sname $ Node.value)
	    +> add env
	| `Let (decls,_) | `LetRec (decls,_) ->
	    decls
	    +> List.map (sname $ Node.value $ fst)
	    +> add env
	| `Int  _ | `String _ | `Bool  _  | `Float _  | `Var _     | `Call _
	| `If   _ | `Block _  | `New _    | `Invoke _ | `SlotRef _ | `SlotSet _ ->
	    env
    end
    begin fun {binding=binding} ->
      function
	  `Var ({Node.value = ([],name)} as node) as e ->
	    if List.mem ([],name) binding then
	      e
	    else if List.mem (current,name) binding then
	      `Var {node with Node.value = (current,name)}
	    else
	      e
	| `Var _
	| `Lambda _  | `Let  _ | `LetRec _ | `Int   _ | `String _ | `Bool   _
	| `Float _   | `Call _ | `If     _ | `Block _ | `New    _ | `Invoke _
	| `SlotRef _ | `SlotSet _ as e ->
	    e
    end
    env expr

let method_scope current env m =
  let env' =
    m.Ast.args
    +> List.map (sname $ Node.value)
    +> add env in
    {m with Ast.body = expr_scope current env' m.Ast.body}

let rec stmt_scope (current,env) =
  function
      `Define (name,body) ->
	let env' =
	  add env [(current,Node.value name)] in
	  (current,env'), `Define(name,expr_scope current env' body)
    | `Class c ->
	let env' =
	  add env [(current, Node.value c.Ast.class_name)] in
	let methods' =
	  List.map (method_scope current env') c.Ast.methods in
	  (current,env'), `Class {c with
				    Ast.methods = methods'}
    | `Expr expr ->
	(current,env),`Expr (expr_scope current env expr)
    | `Module m ->
	let current' =
	  current @ [Node.value @@ m.Ast.module_name] in
	  (current',env),`Module {m with Ast.stmts = snd @@ map_accum_left stmt_scope (current',env) m.Ast.stmts}

let trans s =
  snd @@ map_accum_left stmt_scope ([],empty) s

