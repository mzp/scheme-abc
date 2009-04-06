open Base

type class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
}

type method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: Ast.expr
}

type 'stmt stmt_type =
    [ 'stmt BindCheck.stmt_type
    | `DefineClass  of class_
    | `DefineMethod of method_
    | `DefineStaticMethod of method_ ]

type stmt =
    stmt stmt_type

type program = stmt list

let rec klass2method tbl nss : stmt -> unit =
  function
      `DefineMethod {method_name = name;
		     to_class = {Node.value = klass};
		     args = args;
		     body = body} ->
	Hashtbl.add tbl (nss,klass)
	  {Ast.method_name = `Public name;
	   args = args;
	   body = body}
    | `DefineStaticMethod {method_name = name;
			   to_class = {Node.value = klass};
			   args = args;
			   body = body} ->
	Hashtbl.add tbl (nss,klass)
	  {Ast.method_name = `Static name;
	   args = args;
	   body = body}
    | `Module {ModuleTrans.module_name={Node.value = ns};
	       stmts=stmts} ->
	stmts +> List.iter (klass2method tbl (ns::nss))
    | `Class _ | `Define _ | `Expr _ | `DefineClass _ ->
	()

let klass2methods program =
  let tbl =
    Hashtbl.create 0 in
    List.iter (klass2method tbl []) program;
    tbl

let rec methods : stmt -> string list =
  function
      `DefineMethod {method_name={Node.value = name}}
    | `DefineStaticMethod {method_name={Node.value = name}} ->
	[name]
    | `Module {ModuleTrans.stmts=stmts} ->
	HList.concat_map methods stmts
    | `Class _ | `Define _ | `Expr _ | `DefineClass _->
	[]

let methods_set program =
  PSet.set_of_list @@ HList.concat_map methods program

let call_to_invoke (set,tbl) : Ast.expr -> Ast.expr =
  Ast.map
    (function
	 `Call ((`Var ({Node.value = ("",f)} as node))::obj::args)
	   when PSet.mem f set || InterCode.mem_method f tbl ->
	     `Invoke (obj,Node.lift snd node,args)
       | #Ast.expr as e ->
	   e)

let rec stmt_trans nss tbl set : stmt -> BindCheck.stmt list =
  function
      `DefineClass {class_name={Node.value=name} as klass;
		    super = super;
		    attrs = attrs} ->
	let k =
	  `Class {Ast.class_name=klass;
		  super=super;
		  attrs=attrs;
		  methods=Hashtbl.find_all tbl (nss,name)} in
	  stmt_trans nss tbl set k
    | `DefineMethod _ | `DefineStaticMethod _ ->
	[]
    | `Module ({ModuleTrans.module_name={Node.value = ns};
		stmts=stmts} as m) ->
	[`Module ({m with
		     ModuleTrans.stmts=
		      HList.concat_map (stmt_trans (ns::nss) tbl set) stmts})]
    | `Class _ | `Define _ | `Expr _ as s ->
	[BindCheck.lift (call_to_invoke set) s]

let trans table program =
  let k2m =
    klass2methods program in
  let ms =
    methods_set program in
    program +>  HList.concat_map (stmt_trans [] k2m (ms,table))
