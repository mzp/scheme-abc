open Base

type 'stmt stmt_type =
    [ 'stmt BindCheck.stmt_type
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.sname) *
	Ast.sname list * Ast.expr ]

type stmt =
    stmt stmt_type

type program = stmt list

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty xs

let rec klass2method tbl nss : stmt -> unit =
  function
      `DefineMethod (name,(self,{Node.value = klass}),args,body) ->
	Hashtbl.add tbl (nss,klass) {Ast.method_name=`Public name;
				     args = self::args;
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
      `DefineMethod ({Node.value = name},_,_,_) ->
	[name]
    | `Module {ModuleTrans.stmts=stmts} ->
	HList.concat_map methods stmts
    | `Class _ | `Define _ | `Expr _ | `DefineClass _->
	[]

let methods_set program =
  set_of_list @@ HList.concat_map methods program

let expr_trans (set,tbl) : Ast.expr -> Ast.expr =
  function
      `Call ((`Var ({Node.value = ("",f)} as node))::obj::args)
	when PSet.mem f set || InterCode.mem_method f tbl ->
	`Invoke (obj,Node.lift snd node,args)
    | #Ast.expr as e ->
	e

let rec stmt_trans nss tbl set : stmt -> BindCheck.stmt list =
  function
      `DefineClass ({Node.value=name} as klass,super,attrs) ->
	let k =
	  `Class {ModuleTrans.klass_name=klass;
		  super=super;
		  attrs=attrs;
		  methods=Hashtbl.find_all tbl (nss,name)} in
	  stmt_trans nss tbl set k
    | `DefineMethod _ ->
	[]
    | `Module ({ModuleTrans.module_name={Node.value = ns};
		stmts=stmts} as m) ->
	[`Module ({m with
		     ModuleTrans.stmts=
		      HList.concat_map (stmt_trans (ns::nss) tbl set) stmts})]
    | `Class _ | `Define _ | `Expr _ as s ->
	[BindCheck.lift (Ast.map (expr_trans set)) s]

let trans table program =
  let k2m =
    klass2methods program in
  let ms =
    methods_set program in
    program +>  HList.concat_map (stmt_trans [] k2m (ms,table))
