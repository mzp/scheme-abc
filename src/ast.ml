open Base

(* name := namespace * symbol *)
type name = (string * string) Node.t
type ident = string Node.t

(* expression has no side-effect. *)
type expr = 
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of ident
    | `Lambda  of ident list * expr
    | `Call    of expr list
    | `If      of expr * expr * expr
    | `Let     of (ident*expr) list * expr
    | `LetRec  of (ident*expr) list * expr
    | `Block   of expr list
    | `New     of name * expr list
    | `Invoke  of expr   * ident * expr list
    | `SlotRef of expr * ident
    | `SlotSet of expr * ident * expr ]

(* statement has side-effect *)
type attr    = ident
type method_ = ident * ident list * expr

type stmt = 
    [ `Define of ident * expr
    | `Expr of expr
    | `Class of ident * name * attr list * method_ list ]

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
  
let rec to_string =
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
	Node.to_string (Printf.sprintf "Var %s")  n
    | `Lambda (args,expr') ->
	Printf.sprintf "Lambda (%s,%s)" 
	  (string_of_list @@ List.map (Node.to_string id) args)
	  (to_string expr')
    | `Call exprs ->
	Printf.sprintf "Call %s" @@
	  string_of_list @@ List.map to_string exprs
    | `If (a,b,c) ->
	Printf.sprintf "If (%s,%s,%s)" 
	  (to_string a) (to_string b) (to_string c)
    | `Let (decl,body) ->
	let decl' =
	  string_of_list @@ 
	    List.map (fun (a,b)->
			Printf.sprintf "(%s,%s)" 
			  (Node.to_string id a)
			  (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "Let (%s,%s)" decl' body'
    | `LetRec (decl,body) ->
	let decl' =
	  string_of_list @@ 
	    List.map (fun (a,b)->
			Printf.sprintf "(%s,%s)" 
			  (Node.to_string id a)
			  (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "LetRec (%s,%s)" decl' body'
    | `Block exprs ->
	Printf.sprintf "Block %s" @@ string_of_list @@
	  List.map to_string exprs
    | `New (name,args) ->
	Printf.sprintf "New (%s,%s)"
	  (Node.to_string (fun (a,b) -> a ^ ":" ^ b) name) @@
	  string_of_list @@ List.map to_string args
    | `Invoke (obj,name,args) ->
	Printf.sprintf "Invoke (%s,%s,%s)"
	  (to_string obj)
	  (Node.to_string id name) @@
	  string_of_list @@ List.map to_string args
    | `SlotRef (obj,name) ->
	Printf.sprintf "SlotRef (%s,%s)"
	  (to_string obj) @@ Node.to_string id name
    | `SlotSet (obj,name,value) ->
	Printf.sprintf "SlotSet (%s,%s,%s)"
	  (to_string obj) 
	  (Node.to_string id name)
	  (to_string value)

let to_string_stmt =
  function
      `Define (x,y) ->
	Printf.sprintf "Define (%s,%s)" 
	  (Node.to_string id x) @@ 
	  to_string y
    | `Expr x ->
	Printf.sprintf "Expr (%s)" (to_string x)
    | `Class (name,sname,attrs,body) ->
	Printf.sprintf "Class (%s,%s,%s,%s)"
	  (Node.to_string id name)
	  (Node.to_string (fun (a,b) -> a ^ ":" ^ b) sname)
	  (string_of_list @@ List.map (Node.to_string id) attrs)
	@@ String.concat "\n"
	@@ List.map (fun (name,args,expr) ->
		       Printf.sprintf "((%s %s) %s)"
			 (Node.to_string id name)
			 (String.concat " " @@ 
			    List.map (Node.to_string id) args)
			 (to_string expr))
	  body
