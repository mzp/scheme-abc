open Base

type stmt_term =
    [ ModuleTrans.stmt_term
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.qname) *
	Ast.sname list * Ast.expr ]
type stmt =
    [ stmt_term
    | `Module of Ast.sname * Ast.sname list * stmt list ]


type program = stmt list
type method_info = {
  name: Ast.sname;
  args: Ast.sname list;
  body: Ast.expr
}

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty xs

let methods_table (program : program) =
  let tbl =
    Hashtbl.create 16 in
    program +> List.iter
      (function
           `DefineMethod (name,(self,{Node.value = klass}),args,body) ->
	     Hashtbl.add tbl klass (name,self::args,body)
	 | `DefineClass _  | #BindCheck.stmt ->
	     ());
    tbl

let methods_name_set (program : program) =
  set_of_list @@ HList.concat_map
    (function
         `DefineMethod ({Node.value = name},_,_,_) ->
	   [name]
       | `ExternalClass (_,methods) ->
	   List.map Node.value methods
       | `DefineClass _ | #BindCheck.stmt ->
	   []) program

let expr_trans set : Ast.expr -> Ast.expr =
  function
      `Call ((`Var f)::obj::args) when PSet.mem (snd f.Node.value) set ->
	`Invoke (obj,Node.lift snd f,args)
    | #Ast.expr as e ->
	e

let stmt_trans tbl set : stmt -> BindCheck.stmt list =
  function
      `DefineClass (klass,super,attrs) ->
	[`Class (klass,super,attrs,Hashtbl.find_all tbl klass.Node.value)]
    | `DefineMethod _ ->
	[]
    | #Ast.stmt as s ->
	[(Ast.lift_stmt (Ast.map (expr_trans set)) s :> BindCheck.stmt)]
    | #BindCheck.stmt as s ->
	[s]

let trans program =
  let tbl =
    methods_table program in
  let methods =
    methods_name_set program in
    program +>  HList.concat_map (stmt_trans tbl methods)
