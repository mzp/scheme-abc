open Base

(* name := namespace * symbol *)
type name = string * string

(* expression has no side-effect. *)
type expr = 
    Int of int
  | String of string
  | Bool   of bool
  | Float  of float
  | Var    of string
  | Lambda of string list * expr
  | Call   of expr list
  | If     of expr * expr * expr
  | Let    of (string*expr) list * expr
  | LetRec of (string*expr) list * expr
  | Block  of expr list
  | New    of name * expr list
  | Invoke of expr   * string * expr list (* (invoke <object> <method-name> <arg1> <arg2>...)*)

(* statement has side-effect *)
type stmt = 
  | Define of string * expr
  | Expr of expr
  | Class of string * name * attr list * method_ list
and attr    = string
and method_ = string * string list * expr

type program = stmt list

let lift_stmt f =
  function
      Define (name,expr) ->
	Define (name,f expr)
    | Expr expr ->
	Expr (f expr)
    | Class (name,sname,attrs,body) ->
	let body' =
	  List.map (Core.Tuple.T3.map3 ~f:f) body in
	  Class (name,sname,attrs,body')


let lift_program f = List.map (lift_stmt f)

let rec map f expr =
  let g =
    map f in
    match expr with
	Int _ | String _ | Bool _ | Float _ | Var _ ->
	  f expr
      | Lambda (name,expr') ->
	  f @@ Lambda (name,(g expr'))
      | Call exprs ->
	  f @@ Call (List.map g exprs)
      | If (a,b,c) ->
	  f @@ If ((g a),(g b),(g c))
      | Let (decl,body) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ Let (decl',body')
      | LetRec (decl,body) ->
	  let decl' =
	    List.map (fun (a,b)->(a,g b)) decl in
	  let body' =
	    g body in
	    f @@ LetRec (decl',body')
      | Block exprs' ->
	  f @@ Block (List.map g exprs')
      | New (name,args) ->
	  f @@ New (name,List.map g args)
      | Invoke (obj,name,args) ->
	  f @@ Invoke (g obj,name,List.map g args)
  
let rec to_string =
  function
      Int n ->
	Printf.sprintf "Int %d" n
    | String s ->
	Printf.sprintf "String %s" s
    | Bool b ->
	Printf.sprintf "Bool %s" (if b then "true" else "false")
    | Float d ->
	Printf.sprintf "Float %f" d
    | Var n ->
	Printf.sprintf "Var %s" n
    | Lambda (args,expr') ->
	Printf.sprintf "Lambda ([%s],%s)" (String.concat "; " args) (to_string expr')
    | Call exprs ->
	Printf.sprintf "Call [%s]" @@ String.concat "; " @@ List.map to_string exprs
    | If (a,b,c) ->
	Printf.sprintf "If (%s,%s,%s)" 
	  (to_string a) (to_string b) (to_string c)
    | Let (decl,body) ->
	let decl' =
	  String.concat "; " @@ List.map (fun (a,b)->Printf.sprintf "(%s,%s)" a (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "Let ([%s],%s)" decl' body'
    | LetRec (decl,body) ->
	let decl' =
	  String.concat "; " @@ List.map (fun (a,b)->Printf.sprintf "(%s,%s)" a (to_string b)) decl in
	let body' =
	  to_string body in
	  Printf.sprintf "LetRec (%s,%s)" decl' body'
    | Block exprs ->
	Printf.sprintf "Block [%s]" @@ String.concat "; " @@ List.map to_string exprs
    | New ((ns,name),args) ->
	Printf.sprintf "New (%s:%s,[%s])" ns name @@
	  String.concat "; " @@ List.map to_string args
    | Invoke (obj,name,args) ->
	Printf.sprintf "Invoke (%s,%s,[%s])"
	  (to_string obj)
	  name
	  (String.concat "; " @@ List.map to_string args)

let to_string_stmt =
  function
      Define (x,y) ->
	Printf.sprintf "Define (%s,%s)" x (to_string y)
    | Expr x ->
	Printf.sprintf "Expr (%s)" (to_string x)
    | Class (name,(ns,sname),attrs,body) ->
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
	  body
