open Base

type stmt_term =
    [ ModuleTrans.stmt_term
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.sname) *
	Ast.sname list * Ast.expr ]

type stmt =
    [ stmt_term
    | `Module of Ast.sname * Ast.sname list * stmt list ]

type program = stmt list

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty xs

let rec klass2method tbl nss : stmt -> unit =
  function
      `DefineMethod (name,(self,{Node.value = klass}),args,body) ->
	Hashtbl.add tbl (nss,klass) (name, self::args, body)
    | `Module ({Node.value = ns},_,stmts) ->
	stmts +> List.iter (klass2method tbl (ns::nss))
    | `DefineClass _ | #ModuleTrans.stmt_term ->
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
    | `ExternalClass (_,methods) ->
	List.map Node.value methods
    | `Module (_,_,stmts) ->
	HList.concat_map methods stmts
    | `DefineClass _ | #ModuleTrans.stmt_term ->
	[]

let methods_set program =
  set_of_list @@ HList.concat_map methods program

let expr_trans set : Ast.expr -> Ast.expr =
  function
      `Call ((`Var ({Node.value = ("",f)} as node))::obj::args) when PSet.mem f set ->
	`Invoke (obj,Node.lift snd node,args)
    | #Ast.expr as e ->
	e

let rec stmt_trans nss tbl set : stmt -> ModuleTrans.stmt list =
  function
      `DefineClass ({Node.value=name} as klass,super,attrs) ->
	[`Class (klass,super,attrs,
		 Hashtbl.find_all tbl (nss,name))]
    | `DefineMethod _ ->
	[]
    | `Module ({Node.value = ns} as name,exports,stmts) ->
	[`Module (name,exports,
		  HList.concat_map (stmt_trans (ns::nss) tbl set) stmts)]
    | #ModuleTrans.stmt_term as s ->
	[ModuleTrans.lift (Ast.map (expr_trans set)) s]

let trans program =
  let tbl =
    klass2methods program in
  let set =
    methods_set program in
    program +>  HList.concat_map (stmt_trans [] tbl set)
