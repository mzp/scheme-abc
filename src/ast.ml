open Base

(* name := namespace * symbol *)
type qname = (string * string) Node.t
type sname = string Node.t

(* expression has no side-effect. *)
type expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
    | `Lambda  of sname list * expr
    | `Call    of expr list
    | `If      of expr * expr * expr
    | `Let     of (sname*expr) list * expr
    | `LetRec  of (sname*expr) list * expr
    | `Block   of expr list
    | `New     of qname * expr list
    | `Invoke  of expr   * sname * expr list
    | `SlotRef of expr * sname
    | `SlotSet of expr * sname * expr ]

(* statement has side-effect *)
type stmt_name  =
    [ `Public of qname
    | `Internal of qname]
type attr    = sname
type method_ = sname * sname list * expr

type stmt =
    [ `Define of stmt_name * expr
    | `Expr of expr
    | `Class of stmt_name * qname * attr list * method_ list ]

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

let string_of_qname {Node.value=(ns,name)} =
  ns ^ "." ^ name

let string_of_sname {Node.value=name} =
  name

let rec to_string : expr -> string =
  function
      `Int n ->
	Node.to_string (Printf.sprintf "Int %d") n
    | `String s ->
	Node.to_string (Printf.sprintf "String %s") s
    | `Bool b ->
	Node.to_string
	  (fun b -> if true then "Bool true" else "Bool false")
	  b
    | `Float d ->
	Node.to_string (Printf.sprintf "Float %f") d
    | `Var n ->
	string_of_qname n
    | `Lambda (args,expr') ->
	Printf.sprintf "Lambda (%s,%s)"
	  (string_of_list_by string_of_sname args)
	  (to_string expr')
    | `Call exprs ->
	Printf.sprintf "Call %s" @@
	  string_of_list_by to_string exprs
    | `If (a,b,c) ->
	Printf.sprintf "If (%s,%s,%s)"
	  (to_string a) (to_string b) (to_string c)
    | `Let (decl,body) ->
	let decl' =
	  string_of_list_by
	    (fun (a,b)->
		 Printf.sprintf "(%s,%s)"
		   (string_of_sname a)
		   (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "Let (%s,%s)" decl' body'
    | `LetRec (decl,body) ->
	let decl' =
	  string_of_list_by
	    (fun (a,b)->
	       Printf.sprintf "(%s,%s)"
		 (string_of_sname a)
		 (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "LetRec (%s,%s)" decl' body'
    | `Block exprs ->
	Printf.sprintf "Block %s" @@ string_of_list @@
	  List.map to_string exprs
    | `New (name,args) ->
	Printf.sprintf "New (%s,%s)"
	  (string_of_qname name) @@
	  string_of_list_by to_string args
    | `Invoke (obj,name,args) ->
	Printf.sprintf "Invoke (%s,%s,%s)"
	  (to_string obj)
	  (string_of_sname name) @@
	  string_of_list_by to_string args
    | `SlotRef (obj,name) ->
	Printf.sprintf "SlotRef (%s,%s)"
	  (to_string obj) @@ string_of_sname name
    | `SlotSet (obj,name,value) ->
	Printf.sprintf "SlotSet (%s,%s,%s)"
	  (to_string obj)
	  (string_of_sname name)
	  (to_string value)

let string_of_stmt_name =
  function
      `Public name | `Internal name ->
	string_of_qname name

let to_string_stmt : stmt -> string=
  function
      `Define (x,y) ->
	Printf.sprintf "Define (%s,%s)"
	  (string_of_stmt_name x) @@
	  to_string y
    | `Expr x ->
	Printf.sprintf "Expr (%s)" (to_string x)
    | `Class (klass,super,attrs,body) ->
	Printf.sprintf "Class (%s,%s,%s,%s)"
	  (string_of_stmt_name klass)
	  (string_of_qname super)
	  (string_of_list_by string_of_sname attrs)
	@@ String.concat "\n"
	@@ List.map (fun (name,args,expr) ->
		       Printf.sprintf "((%s %s) %s)"
			 (string_of_sname name)
			 (String.concat " " @@
			    List.map string_of_sname args)
			 (to_string expr))
	  body
