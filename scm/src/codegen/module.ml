open Base
open Node

type 'expr expr = 'expr Ast.expr

type stmt_name  =
    [ `Public of Ast.qname
    | `Internal of Ast.qname]

type ('expr,'stmt) stmt =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (stmt_name,'expr) Ast.class_ ]

let fold f g fold_rec env expr =
  Ast.fold f g fold_rec env expr

let lift f =
  function
      `Define (name,expr) ->
	`Define (name,f expr)
    | `Expr expr ->
	`Expr (f expr)
    | `Class c ->
	open Ast in
        let methods' =
	  c.methods +>
	    List.map (fun m -> {m with body = f m.body}) in
	  `Class {c with
		    methods = methods'}

let fold_stmt f g env =
  function
      `Define _ | `Expr _ | `Class _ as s ->
	g (f env s) s

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list


let to_stmt_name exports ns name =
  let qname =
    Node.lift (fun name -> ns,name) name in
  match exports with
      `All ->
	`Public qname
    | `Only publics ->
	if List.exists (fun {value=v} -> name.value = v) publics then
	  `Public qname
	else
	  `Internal qname

let rec expand_module ns exports stmt =
  open Ast in
  match stmt with
      `Class c ->
	[`Class { c with
		    class_name = to_stmt_name exports ns c.class_name
		}]
    | `Define (name,body) ->
	[`Define (to_stmt_name exports ns name,body)]
    | `Expr _ as expr ->
	[expr]
    | `Module m ->
	m.stmts
	+> HList.concat_map (expand_module (ns@[m.module_name.value]) m.exports)

let of_ast =
  HList.concat_map (expand_module [] `All)

