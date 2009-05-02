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

let (++) ns ({Node.value=name} as loc) =
  {loc with
     Node.value = (ns,name)}

let access exports ns name =
  let qname =
    ns ++ name in
  match exports with
      `All ->
	`Public qname
    | `Only names ->
	if List.exists (fun {Node.value=v} -> name.Node.value = v) names then
	  `Public qname
	else
	  `Internal qname

let rec expand_module ns exports =
  function
      `Class ({Ast.class_name=klass} as c) ->
	[`Class {c with
		   Ast.class_name = access exports ns klass;
		}]
    | `Define (name,body) ->
	[`Define (access exports ns name,body)]
    | `Expr _ as expr ->
	[expr]
    | `Module {Ast.module_name={Node.value=name}; exports=exports; stmts=stmts} ->
	HList.concat_map (expand_module (ns@[name]) exports) stmts

let trans : Ast.program -> program =
  HList.concat_map (expand_module [] `All)

