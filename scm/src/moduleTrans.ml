open Base

type exports =
    All
  | Restrict of Ast.sname list

type klass_type = {
  klass_name : Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
  methods: Ast.method_ list
}

type 'stmt module_type = {
  module_name : Ast.sname;
  exports : exports;
  stmts   : 'stmt list
}

type 'stmt stmt_type =
    [ `Class  of klass_type
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
      `Class {klass_name=klass;super=super;attrs=attrs;methods=methods} ->
	[`Class {
	   Ast.klass_name = access exports ns klass;
	   super = super;
	   attrs = attrs;
	   methods = methods
	 }]
    | `Define (name,body) ->
	[`Define (access exports ns name,body)]
    | `Expr _ as expr ->
	[expr]
    | `Module {module_name={Node.value=name}; exports=exports; stmts=stmts} ->
	HList.concat_map (trans_stmt (ns@[name]) exports) stmts

let rec lift f : stmt -> stmt =
  function
      `Class ({methods=methods} as k) ->
	`Class {k with
		  methods = methods +> List.map (fun ({Ast.body=body} as m) ->
						   {m with
						      Ast.body = f body
						   })
	       }
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
