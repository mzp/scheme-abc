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

type ('expr,'stmt) expr_stmt_type =
    [ `Define of Ast.sname * 'expr
    | `Expr   of 'expr ]

type ('expr,'stmt) module_stmt_type =
    [ `Module of 'stmt module_type ]

type ('expr,'stmt) stmt_type =
    [ `Class  of (Ast.sname, 'expr) Ast.class_type
    | ('expr,'stmt) expr_stmt_type
    | ('expr,'stmt) module_stmt_type ]

(* ------------------------------
   fold/lift function
   ------------------------------ *)
let fold f g fold_rec env e =
  Ast.fold f g fold_rec env e

let fold_expr_stmt f g env =
  function
      `Define _ | `Expr _ as s ->
	g (f env s) s

let fold_module_stmt f g fold_rec env =
  function
      `Module m as s ->
	let env' =
	  f env s in
	  g env' @@ `Module {m with stmts = List.map (fold_rec env') m.stmts}

let fold_stmt f g fold_rec env : ('a,'b) stmt_type -> 'c =
  function
      `Class _ as s ->
	g (f env s) s
    | #expr_stmt_type as s ->
	fold_expr_stmt f g env s
    | #module_stmt_type as s ->
	fold_module_stmt f g fold_rec env s

let lift_expr f =
  function
      `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)

let lift_module f lift_rec =
  function
      `Module m ->
	`Module {m with
		   stmts = List.map lift_rec m.stmts
		}

let rec lift f lift_rec =
  function
    | `Class ({Ast.methods=methods} as c) ->
	`Class {c with
		  Ast.methods = methods +> List.map (fun ({Ast.body=body}as m) ->
						       {m with Ast.body= f body})}
    | #expr_stmt_type as s ->
	lift_expr f s
    | #module_stmt_type as s ->
	lift_module f lift_rec s

(* ------------------------------
   module expand
   ------------------------------*)
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

let rec fold_stmt' f g env stmt =
  fold_stmt f g (fold_stmt' f g) env stmt

let public_symbols s =
  fold_stmt'
    const
    begin fun e s ->
      match s with
	  `Define (name,_) ->
	    [name]
	| `Expr _ | `Module _| `Class _ ->
	    []
    end [] s


let public_methods _ = []

