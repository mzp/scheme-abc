open Base

type stmt = 
    [ Ast.stmt 
    | `DefineClass  of ident * Ast.name * ident list
    | `DefineMethod of ident * (ident * ident) * ident list * Ast.expr]
and attr = string Node.t
and ident = string Node.t

type program = stmt list
type method_info = {
  name: ident;
  args: ident list;
  body: Ast.expr
}

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty xs

let methods_table program =
  let tbl =
    Hashtbl.create 16 in
    program +> List.iter
      (function
           `DefineMethod (name,(self,{Node.value = klass}),args,body) ->
	     Hashtbl.add tbl klass (name,self::args,body)
	 | _ ->
	     ());
    tbl

let methods_name_set program =
  set_of_list @@ HList.concat_map 
    (function
         `DefineMethod ({Node.value = name},_,_,_) ->
	   [name]
       | _ ->
	   []) program

let expr_trans set =
  function
      `Call ((`Var f)::obj::args) when PSet.mem f.Node.value set ->
	`Invoke (obj,f,args)
    | e ->
	e

let stmt_trans tbl set =
  function
      #Ast.stmt as stmt ->
	[Ast.lift_stmt (Ast.map @@ expr_trans set) stmt]
    | `DefineClass (klass,super,attrs) ->
	[`Class (klass,super,attrs,Hashtbl.find_all tbl klass.Node.value)]
    | `DefineMethod _ ->
	[]

let trans program =
  let tbl =
    methods_table program in
  let methods =
    methods_name_set   program in
    program +>  HList.concat_map (stmt_trans tbl methods)

let to_string =
  function
      #Ast.stmt as stmt ->
	Ast.to_string_stmt stmt
    | `DefineClass (name,super,attrs) ->
	Printf.sprintf "Class (%s,%s,%s)"
	  (Node.to_string id name)
	  (Node.to_string (fun (a,b) -> a^":"^b) super) @@
	  string_of_list @@ List.map (Node.to_string id) attrs
    | `DefineMethod (f,(self,klass),args,body) ->
	let show =
	  Node.to_string id in
	  Printf.sprintf "Metod (%s,((%s %s) %s),%s)" 
	  (show f) (show self) (show klass) 
	  (string_of_list (List.map show args)) (Ast.to_string body)
