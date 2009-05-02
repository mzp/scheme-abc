open Base

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
	let methods' =
	  c.Ast.methods +>
	    List.map (fun m -> {m with Ast.body = f m.Ast.body}) in
	  `Class {c with
		    Ast.methods = methods'}

let fold_stmt f g env : 'a stmt_type -> 'b =
  function
      `Define _ | `Expr _ | `Class _ as s ->
	g (f env s) s

let rec fold' f g env expr =
  fold f g (fold' f g) env expr

let map f expr =
  fold'
    (flip const)
    (fun _ b -> f b)
    expr expr
