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
type stmt_name  =
    [ `Public of qname
    | `Internal of qname]
type attr    = sname
type 'expr method_type = sname * sname list * 'expr

type 'expr stmt_type =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of stmt_name * qname * attr list * 'expr method_type list ]

type method_ =
    expr method_type
type stmt =
    expr stmt_type

type program = stmt list

let lift_stmt f =
  function
      `Define (name,expr) ->
	`Define (name,f expr)
    | `Expr expr ->
	`Expr (f expr)
    | `Class (name,sname,attrs,body) ->
	let body' =
	  List.map (Tuple.T3.map3 f) body in
	  `Class (name,sname,attrs,body')

let lift_program f = List.map (lift_stmt f)

let rec map f expr =
  let g =
    map f in
    match expr with
	`Int _ | `String _ | `Bool _ | `Float _ | `Var _ ->
	  f expr
      | `Lambda (name,expr') ->
	  f @@ `Lambda (name,(g expr'))
      | `Call exprs ->
	  f @@ `Call (List.map g exprs)
      | `If (a,b,c) ->
	  f @@ `If ((g a),(g b),(g c))
      | `Let (decl,body) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ `Let (decl',body')
      | `LetRec (decl,body) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ `LetRec (decl',body')
      | `Block exprs' ->
	  f @@ `Block (List.map g exprs')
      | `New (name,args) ->
	  f @@ `New (name,List.map g args)
      | `Invoke (obj,name,args) ->
	  f @@ `Invoke (g obj,name,List.map g args)
      | `SlotRef (obj,name) ->
	  f @@ `SlotRef (g obj,name)
      | `SlotSet (obj,name,value) ->
	  f @@ `SlotSet (g obj,name,g value)
