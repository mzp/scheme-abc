open Base

type exports =
    All
  | Restrict of Ast.sname list

type 'stmt module_type = {
  module_name : Ast.sname;
  exports : exports;
  stmts   : 'stmt list
}

type 'stmt stmt_type =
    [ `Class  of (Ast.sname,Ast.expr) Ast.class_type
    | `Define of Ast.sname * Ast.expr
    | `Expr   of Ast.expr
    | `Module of 'stmt module_type ]

type stmt =
    stmt stmt_type

type program = stmt list

let (++) ns ({Node.value=name} as loc) =
  {loc with
     Node.value = (String.concat "." ns,name)}

let access exports ns name =
  let qname =
    ns ++ name in
  match exports with
      All ->
	`Public qname
    | Restrict names ->
	if List.exists (fun {Node.value=v} -> name.Node.value = v) names then
	  `Public qname
	else
	  `Internal qname

let rec trans_stmt ns exports : stmt -> Ast.stmt list =
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
	HList.concat_map (trans_stmt (ns@[name]) exports) stmts

let rec lift f : stmt -> stmt =
  function
      `Class ({Ast.methods=methods} as k) ->
	let methods' =
	  methods +> List.map
	    (fun ({Ast.body=body} as m) ->
	       {m with Ast.body = f body}) in
	  `Class {k with
		    Ast.methods = methods' }
    | `Define (name,body) ->
	`Define (name,f body)
    | `Expr expr ->
	`Expr (f expr)
    | `Module ({stmts=stmts} as m) ->
	`Module {m with
		   stmts = List.map (lift f) stmts
		}

let trans =
  HList.concat_map (trans_stmt [] All)
