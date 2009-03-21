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

type method_name =
    [ `Public of sname
    | `Static of sname ]

type 'expr method_type = {
  method_name : method_name;
  args : sname list;
  body : 'expr;
}

type 'expr class_type = {
  klass_name : stmt_name;
  super: qname;
  attrs: attr list;
  methods: 'expr method_type list
}

type 'expr stmt_type =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of 'expr class_type ]

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

let map f expr =
  fold_up f f expr
