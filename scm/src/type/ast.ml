open Base

type qname = (string list * string) Node.t
type sname = string Node.t

type 'expr expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
    | `Lambda  of sname list * 'expr
    | `Call    of 'expr list
    | `If      of 'expr * 'expr * 'expr
    | `Let     of (sname*'expr) list * 'expr
    | `LetRec  of (sname*'expr) list * 'expr
    | `Block   of 'expr list
    | `New     of qname * 'expr list
    | `Invoke  of 'expr   * sname * 'expr list
    | `SlotRef of 'expr * sname
    | `SlotSet of 'expr * sname * 'expr ]

type 'stmt module_ = {
  module_name : sname;
  exports : [`All | `Only of sname list];
  stmts   : 'stmt list
}
type ('expr,'stmt) module_stmt =
    [ `Module of 'stmt module_ ]

type 'expr method_ = {
  method_name : [`Public of sname | `Static of sname];
  args : sname list;
  body : 'expr;
}

type ('name,'expr) class_ = {
  class_name : 'name;
  super: qname;
  attrs: sname list;
  methods: 'expr method_ list
}

type ('expr,'stmt) expr_stmt =
    [ `Define of sname * 'expr
    | `Expr   of 'expr ]

type ('expr,'stmt) class_stmt =
    [ `Class  of (sname, 'expr) class_ ]

type ('expr,'stmt) stmt =
    [ ('expr,'stmt) class_stmt
    | ('expr,'stmt) expr_stmt
    | ('expr,'stmt) module_stmt ]

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list

let fold f g fold_rec env =
  function
    | `Bool _ | `Float _ | `Int _ |  `String _ | `Var _ as e ->
	g (f env e) e
    | `Lambda (args, body) as e ->
	let env' =
	  f env e in
	  g env' @@ `Lambda (args, fold_rec env' body)
    | `Call exprs as e ->
	let env' =
	  f env e in
	  g env' @@ `Call (List.map (fold_rec env') exprs)
    | `If (a,b,c) as e ->
	let env' =
	  f env e in
	  g env' @@ `If (fold_rec env' a, fold_rec env' b, fold_rec env' c)
    | `Let (decl,body) as e ->
	let env' =
	  f env e in
	let decl' =
	  List.map (Tuple.T2.map2 (fold_rec env)) decl in
	let body' =
	  fold_rec env' body in
	  g env' @@ `Let (decl',body')
    | `LetRec (decl,body) as e ->
	let env' =
	  f env e in
	let decl' =
	  List.map (Tuple.T2.map2 (fold_rec env')) decl in
	let body' =
	  fold_rec env' body in
	  g env' @@ `LetRec (decl',body')
    | `Block exprs' as e ->
	let env' =
	  f env e in
	  g env' @@ `Block (List.map (fold_rec env') exprs')
    | `New (name,args) as e ->
	let env' =
	  f env e in
	  g env' @@ `New (name,List.map (fold_rec env') args)
    | `Invoke (obj,name,args) as e ->
	let env' =
	  f env e in
	  g env' @@ `Invoke (fold_rec env' obj,
			     name,
			     List.map (fold_rec env') args)
    | `SlotRef(obj,name) as e ->
	let env' =
	  f env e in
	  g env' @@ `SlotRef (fold_rec env' obj, name)
    | `SlotSet (obj,name,value) as e ->
	let env' =
	  f env e in
	  g env' @@ `SlotSet (fold_rec env' obj, name, fold_rec env' value)

let fold_expr_stmt f g env =
  function
      `Define _ | `Expr _ as s ->
	g (f env s) s

let fold_module_stmt f g fold_rec env =
  function
      `Module m as s ->
	let env' =
	  f env s in
	  g env' @@ `Module {m with stmts = List.map (fold_rec env') m.stmts}

let fold_class_stmt f g env =
  function
      `Class _ as s ->
	g (f env s) s

let fold_stmt f g fold_rec env =
  function
      #class_stmt as s ->
	fold_class_stmt f g env s
    | #expr_stmt as s ->
	fold_expr_stmt f g env s
    | #module_stmt as s ->
	fold_module_stmt f g fold_rec env s

(* ------------------------------
   lift: lift up (expr->expr) function to (stmt->stmt) function
   ------------------------------ *)
let lift_expr f =
  function
      `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)

let lift_module f lift_rec =
  function
      `Module m ->
	`Module {m with
		   stmts = List.map lift_rec m.stmts
		}

let lift_class f =
  function
      `Class ({methods=methods} as c) ->
	`Class {c with
		  methods = methods +> List.map (fun ({body=body}as m) ->
						   {m with body= f body})}

let rec lift f lift_rec =
  function
      #class_stmt as s ->
	lift_class f s
    | #expr_stmt as s ->
	lift_expr f s
    | #module_stmt as s ->
	lift_module f lift_rec s
