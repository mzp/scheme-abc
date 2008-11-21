open Base

(* name := namespace * symbol *)
type name = string * string

(* expression has no side-effect. *)
type expr = 
    Int of int Node.t
  | String of string Node.t
  | Bool   of bool Node.t
  | Float  of float Node.t
  | Var    of string Node.t
  | Lambda of (string list * expr) Node.t
  | Call   of expr list Node.t
  | If     of (expr * expr * expr) Node.t
  | Let    of ((string*expr) list * expr) Node.t
  | LetRec of ((string*expr) list * expr) Node.t
  | Block  of expr list Node.t
  | New    of (name * expr list) Node.t
  | Invoke of (expr * string * expr list) Node.t
  | SlotRef of (expr * string) Node.t
  | SlotSet of (expr * string * expr) Node.t


(* statement has side-effect *)
type attr    = string
type method_ = string * string list * expr
type stmt = 
  | Define of (string * expr) Node.t
  | Expr of expr Node.t
  | Class of (string * name * attr list * method_ list) Node.t

type program = stmt list

let lift_stmt f =
  function
      Define ({Node.value = (name,expr)} as node) ->
	Define {node with Node.value = (name,f expr)}
    | Expr ({Node.value = expr} as node) ->
	Expr {node with Node.value = f expr}
    | Class ({Node.value = (name,sname,attrs,body)} as node) ->
	let body' =
	  List.map (Tuple.T3.map3 f) body in
	  Class {node with Node.value = (name,sname,attrs,body')}

let lift_program f = List.map (lift_stmt f)
let rec map f expr =
  let g =
    map f in
    match expr with
	Int _ | String _ | Bool _ | Float _ | Var _ ->
	  f expr
      | Lambda ({Node.value = (name,expr')} as node) ->
	  f @@ Lambda {node with Node.value = (name,(g expr'))}
      | Call ({Node.value = exprs} as node) ->
	  f @@ Call {node with Node.value = List.map g exprs}
      | If ({Node.value = (a,b,c)} as node) ->
	  f @@ If {node with Node.value = (g a),(g b),(g c)}
      | Let ({Node.value = (decl,body)} as node) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ Let {node with Node.value = (decl',body')}
      | LetRec ({Node.value = (decl,body)} as node) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ LetRec {node with Node.value = (decl',body')}
      | Block ({Node.value = exprs'} as node) ->
	  f @@ Block {node with Node.value = (List.map g exprs')}
      | New ({Node.value = (name,args)} as node) ->
	  f @@ New {node with Node.value = (name,List.map g args)}
      | Invoke ({Node.value = (obj,name,args)} as node) ->
	  f @@ Invoke {node with Node.value = (g obj,name,List.map g args)}
      | SlotRef ({Node.value = (obj,name)} as node) ->
	  f @@ SlotRef {node with Node.value = (g obj,name)}
      | SlotSet ({Node.value = (obj,name,value)} as node) ->
	  f @@ SlotSet {node with Node.value = (g obj,name,g value)}
  
let rec to_string =
  function
      Int n ->
	Node.to_string (Printf.sprintf "Int %d") n
    | String s ->
	Node.to_string (Printf.sprintf "String %s") s
    | Bool b ->
	Node.to_string (fun b -> if b then "Bool true" else "Bool false") b
    | Float d ->
	Node.to_string (Printf.sprintf "Float %f") d
    | Var n ->
	Node.to_string (Printf.sprintf "Var %s") n
    | Lambda n ->
	Node.to_string 
	  (fun (args,body) ->
	     Printf.sprintf "Lambda ([%s],%s)" 
	       (String.concat "; " args) 
	       (to_string body)) n
    | Call n ->
	Node.to_string 
	  (fun exprs ->
	     Printf.sprintf "Call [%s]" @@ String.concat "; " @@ 
	       List.map to_string exprs)
	  n
    | If n ->
	Node.to_string
	  (fun (a,b,c) ->
	     Printf.sprintf "If (%s,%s,%s)" 
	       (to_string a) (to_string b) (to_string c))
	  n
    | Let n ->
	Node.to_string
	  (fun(decl,body) ->
	     let decl' =
	       String.concat "; " @@ 
		 List.map (fun (a,b) -> 
			     Printf.sprintf "(%s,%s)" a (to_string b)) decl in
	     let body' =
	       to_string body in
	       Printf.sprintf "Let ([%s],%s)" decl' body')
	  n
    | LetRec n ->
	Node.to_string
	  (fun(decl,body) ->
	     let decl' =
	       String.concat "; " @@ 
		 List.map (fun (a,b) -> 
			     Printf.sprintf "(%s,%s)" a (to_string b)) decl in
	     let body' =
	       to_string body in
	       Printf.sprintf "LetRec ([%s],%s)" decl' body')
	  n
    | Block n ->
	Node.to_string
	  (fun exprs ->
	     Printf.sprintf "Block [%s]" @@ 
	       String.concat "; " @@ List.map to_string exprs)
	  n
    | New n ->
	Node.to_string
	  (fun	((ns,name),args) ->
	     Printf.sprintf "New (%s:%s,[%s])" ns name @@
	       String.concat "; " @@ List.map to_string args)
	  n
    | Invoke n ->
	Node.to_string
	  (fun (obj,name,args) ->
	     Printf.sprintf "Invoke (%s,%s,[%s])"
	       (to_string obj)
	       name
	       (String.concat "; " @@ List.map to_string args))
	  n
    | SlotRef n ->
	Node.to_string
	  (fun (obj,name) ->
	     Printf.sprintf "SlotRef (%s,%s)"
	       (to_string obj) name)
	  n
    | SlotSet n ->
	Node.to_string
	  (fun (obj,name,value) ->
	     Printf.sprintf "SlotSet (%s,%s,%s)"
	       (to_string obj) name (to_string value))
	  n

let to_string_stmt =
  function
      Define n ->
	Node.to_string
	  (fun (x,y) ->
	     Printf.sprintf "Define (%s,%s)" x (to_string y))
	  n
    | Expr n ->
	Node.to_string
	  (fun x ->
	     Printf.sprintf "Expr (%s)" (to_string x))
	  n
    | Class n ->
	Node.to_string
	  (fun (name,(ns,sname),attrs,body) ->
	     Printf.sprintf "Class (%s,%s::%s,%s,%s)"
	       name
	       ns sname
	       (string_of_list attrs)
	     @@ String.concat "\n"
	     @@ List.map (fun (name,args,expr) ->
			    Printf.sprintf "((%s %s) %s)"
			      name
			      (String.concat " " args)
			      (to_string expr))
	       body)
	  n
