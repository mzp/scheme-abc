open Base

(* ------------------------------
   Types of Ast
   ------------------------------ *)
type 'stmt module_type = {
  module_name : Ast.sname;
  exports : [`All | `Only of Ast.sname list];
  stmts   : 'stmt list
}
type 'a expr_type = 'a Ast.expr_type

type ('expr,'stmt) stmt_type =
    [ `Class  of (Ast.sname, 'expr) Ast.class_type
    | `Define of Ast.sname * 'expr
    | `Expr   of 'expr
    | `Module of 'stmt module_type ]

(* ------------------------------
   fold/lift function
   ------------------------------ *)
let fold f g fold_rec env e =
  Ast.fold f g fold_rec env e

let fold_stmt f g fold_rec env : ('a,'b) stmt_type -> 'c =
  function
      `Class _ | `Define _ | `Expr _ as s ->
	g (f env s) s
    | `Module m as s ->
	let env' =
	  f env s in
	  g env' @@ `Module {m with stmts = List.map (fold_rec env) m.stmts}

let rec lift f lift_rec =
  function
    | `Class ({Ast.methods=methods} as c) ->
	`Class {c with
		  Ast.methods = methods +> List.map (fun ({Ast.body=body}as m) ->
						       {m with Ast.body= f body})}
    | `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)
    | `Module m ->
	`Module {m with
		   stmts = List.map lift_rec m.stmts
		}

let (++) ns ({Node.value=name} as loc) =
  {loc with
     Node.value = (String.concat "." ns,name)}

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

type expr =
    expr expr_type

type stmt =
    (expr,stmt) stmt_type

type program = stmt list

let rec expand_module ns exports : stmt -> Ast.stmt list =
  function
      `Class ({Ast.class_name=klass} as c) ->
	[`Class {c with
		   Ast.class_name = access exports ns klass;
		}]
    | `Define (name,body) ->
	[`Define (access exports ns name,body)]
    | `Expr _ as expr ->
	[expr]
    | `Module {module_name={Node.value=name}; exports=exports; stmts=stmts} ->
	HList.concat_map (expand_module (ns@[name]) exports) stmts

let trans =
  HList.concat_map (expand_module [] `All)
