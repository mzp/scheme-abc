open Base
open Node

type qname = (string list * string) Node.t
type sname = string Node.t

type 'expr expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
    | `Array   of 'expr list
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
    [ `Module of 'stmt module_
    | `Open   of string list Node.t ]

type 'expr method_ = {
  method_name : [`Public of sname | `Static of sname];
  args : sname list;
  body : 'expr;
}

type ('name,'method_) class_ = {
  class_name : 'name;
  super: qname;
  attrs: sname list;
  methods: 'method_ list
}
type ('expr,'stmt) expr_stmt =
    [ `Define of sname * 'expr
    | `Expr   of 'expr ]

type ('expr,'stmt) class_stmt =
    [ `Class  of (sname, 'expr method_) class_ ]

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
    | `Array exprs as e ->
	let env' =
	  f env e in
	  g env' @@ `Array (List.map (fold_rec env') exprs)
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
    | `Open _ as s ->
	g (f env s) s

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

let lift_module _ lift_rec =
  function
      `Module m ->
	`Module {m with
		   stmts = List.map lift_rec m.stmts
		}
    | `Open s ->
	`Open s

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

let fix_lift lift f x =
  let rec lift' f x =
    lift f (lift' f) x in
    lift' f x

let fix_fold fold f g env x =
  let rec fold' f g env x =
    fold f g (fold' f g) env x in
    fold' f g env x

let map fold f expr =
  fix_fold fold (flip const) (fun _ b -> f b) expr expr

let append ns x =
  Node.lift (fun (a,b)-> (ns::a,b)) x

let public_symbols stmt =
  fix_fold fold_stmt const
    begin fun _ stmt ->
      match stmt with
	  `Define (name,_) | `Class {class_name=name} ->
	    [{name with value = ([], name.value)}]
	| `Module {module_name = {value = ns}; exports=`All; stmts=stmts} ->
	    stmts
	    +> List.concat
	    +> List.map (append ns)
	| `Module {module_name = {value = ns}; exports=`Only xs} ->
	    List.map (fun x -> {x with value = ([ns], x.value)}) xs
	| `Expr _ | `Open _ ->
	    []
    end None stmt

let public_modules stmt =
  fix_fold fold_stmt const
    begin fun _ stmt ->
      match stmt with
	| `Module {module_name = name; stmts=stmts} ->
	    stmts
	    +> List.concat
	    +> List.map (Node.lift (fun x-> (Node.value name)::x))
	    +> (fun xs -> (Node.lift (fun x->[x]) name)::xs)
	| `Define _ | `Class _ | `Expr _ | `Open _ ->
	    []
    end None stmt

let public_methods stmt =
  fix_fold fold_stmt const
    begin fun _ stmt ->
      match stmt with
	  `Class {methods=methods} ->
	    List.map
	      (function {method_name=`Public name} |
		        {method_name=`Static name} ->
			  name)
	      methods
	| `Module { stmts = stmts } ->
	    List.concat stmts
	| `Expr _ | `Define _ | `Open _ ->
	  []
    end None stmt
