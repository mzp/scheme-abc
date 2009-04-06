open Base

(* name := namespace * symbol *)
type qname = (string * string) Node.t
type sname = string Node.t

(* expression has no side-effect. *)
type 'expr expr_type =
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

type expr =
    expr expr_type

(* statement has side-effect *)
type method_name =
    [ `Public of sname
    | `Static of sname ]
type 'expr method_type = {
  method_name : method_name;
  args : sname list;
  body : 'expr;
}

type stmt_name  =
    [ `Public of qname
    | `Internal of qname]
type attr    =
    sname
type ('name,'expr) class_type = {
  class_name : 'name;
  super: qname;
  attrs: attr list;
  methods: 'expr method_type list
}

type 'expr stmt_type =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (stmt_name,'expr) class_type ]

type method_ =
    expr method_type
type stmt =
    expr stmt_type
type program =
    stmt list

let lift_stmt f =
  function
      `Define (name,expr) ->
	`Define (name,f expr)
    | `Expr expr ->
	`Expr (f expr)
    | `Class ({methods=methods} as klass) ->
	`Class {klass with
		  methods = methods +> List.map (fun ({body=body}as m) ->
						   {m with body= f body})}

let lift_program f = List.map (lift_stmt f)

let rec fold_up =
  fun branch leaf expr ->
    let g e =
      fold_up branch leaf e in
      match expr with
	  `Int _ | `String _ | `Bool _ | `Float _ | `Var _ ->
	    leaf expr
	| `Lambda (name,expr') ->
	    branch @@ `Lambda (name,(g expr'))
	| `Call exprs ->
	    branch @@ `Call (List.map g exprs)
	| `If (a,b,c) ->
	    branch @@ `If ((g a),(g b),(g c))
	| `Let (decl,body) ->
	    let decl' =
	      List.map (fun (a,b)->(a,g b)) decl in
	    let body' =
	      g body in
	      branch @@ `Let (decl',body')
	| `LetRec (decl,body) ->
	    let decl' =
	      List.map (fun (a,b)->(a,g b)) decl in
	    let body' =
	      g body in
	      branch @@ `LetRec (decl',body')
	| `Block exprs' ->
	    branch @@ `Block (List.map g exprs')
	| `New (name,args) ->
	    branch @@ `New (name,List.map g args)
	| `Invoke (obj,name,args) ->
	    branch @@ `Invoke (g obj,name,List.map g args)
	| `SlotRef (obj,name) ->
	    branch @@ `SlotRef (g obj,name)
	| `SlotSet (obj,name,value) ->
	    branch @@ `SlotSet (g obj,name,g value)

let rec fold f g fold_rec env =
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
	  g env' @@ `Invoke (fold_rec env' obj, name, List.map (fold_rec env') args)
    | `SlotRef(obj,name) as e ->
	let env' =
	  f env e in
	  g env' @@ `SlotRef (fold_rec env' obj, name)
    | `SlotSet (obj,name,value) as e ->
	let env' =
	  f env e in
	  g env' @@ `SlotSet (fold_rec env' obj, name, fold_rec env' value)

let rec fold' f g env expr =
  fold f g (fold' f g) env expr

let map f expr =
  fold'
    (flip const)
    (fun _ b -> f b)
    expr expr
