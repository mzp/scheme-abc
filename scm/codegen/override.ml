open Base

type 'expr expr = 'expr Binding.expr

type 'expr method_ = ('expr Ast.method_) * [`Override] list
type ('expr,'stmt) stmt =
    [ `Define of Module.stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (Module.stmt_name,'expr method_) Ast.class_ ]


let fold f g fold_rec env expr =
  Binding.fold f g fold_rec env expr

let lift f =
  function
      `Class c ->
        let methods' =
	  c.Ast.methods +>
	    List.map (fun (m,attr) -> ({m with Ast.body = f m.Ast.body}, attr)) in
	  `Class {c with
		    Ast.methods = methods'}
    | `Define (name,expr) ->
	`Define (name,f expr)
    | `Expr expr ->
	`Expr (f expr)


let fold_stmt f g env =
  function
      `Define _ | `Expr _ | `Class _ as s  ->
	g (f env s) s

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list

(* ------------------------------ *)
type class_name  = string list * string
type method_name = string
type entry = {
  super : class_name;
  methods : method_name list;
}

type ctx = (class_name * entry) list

(* register class to ctx *)
let of_stmt_name =
  function `Public name | `Internal name ->
    Node.value name

let method_name m =
  match m.Ast.method_name with
      `Public name | `Static name ->
	Node.value name

let add_class (ctx : ctx) {Ast.class_name = class_name;
			   super   = super;
			   methods = methods} : ctx =
  let entry =
    { super   = Node.value super;
      methods = List.map method_name methods} in
    (of_stmt_name class_name, entry)::ctx

(* override is needed or not needed *)
let rec mem_methods (ctx : ctx) class_ method_ =
  match assoc class_ ctx with
      Some {super=super; methods=methods} ->
	if List.mem method_ methods then
	  true
	else
	  mem_methods ctx super method_
    | None ->
	false

let method_attr (ctx : ctx) super method_ =
  let name =
    method_name method_ in
  let attrs =
    if name = "init" then
      []
    else if mem_methods ctx super name then
      [`Override]
    else
      []
  in
    method_,attrs

let update_class (ctx : ctx) c =
  let methods =
    c.Ast.methods +> List.map (method_attr ctx (Node.value c.Ast.super)) in
    {c with Ast.methods = methods}

(* trans *)
let trans ctx stmt =
  let ctx' =
    match stmt with
	`Class c ->
	  add_class ctx c
      | `Define _ | `Expr _ ->
	  ctx in
  let stmt' =
    match stmt with
	`Class c ->
	  (* use ctx, not ctx' *)
	  `Class (update_class ctx c)
      | `Define _ | `Expr _  as s ->
	  s in
    ctx',stmt'

let of_binding stmt =
  snd @@ map_accum_left trans [] stmt
