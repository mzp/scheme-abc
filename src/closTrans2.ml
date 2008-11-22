open Base

type stmt = 
    Plain of Ast2.stmt
  | DefineClass  of ident * Ast2.name * ident list
  | DefineMethod of ident * (ident * ident) * ident list * Ast2.expr
and attr = string Node.t
and ident = string Node.t

type program = stmt list
type method_info = {
  name: ident;
  args: ident list;
  body: Ast2.expr
}

let set_of_list xs =
  List.fold_left (flip PSet.add) PSet.empty xs

let methods_table program =
  let tbl =
    Hashtbl.create 16 in
    program +> List.iter
      (function
           DefineMethod (name,(self,{Node.value = klass}),args,body) ->
	     Hashtbl.add tbl klass (name,self::args,body)
	 | _ ->
	     ());
    tbl

let methods_name_set program =
  set_of_list @@ HList.concat_map 
    (function
         DefineMethod ({Node.value = name},_,_,_) ->
	   [name]
       | _ ->
	   []) program

let expr_trans set =
  function
      Ast2.Call ((Ast2.Var f)::obj::args) when PSet.mem f.Node.value set ->
	Ast2.Invoke (obj,f,args)
    | e ->
	e

let stmt_trans tbl set =
  function
      Plain stmt ->
	[Ast2.lift_stmt (Ast2.map @@ expr_trans set) stmt]
    | DefineClass (klass,super,attrs) ->
	[Ast2.Class (klass,super,attrs,Hashtbl.find_all tbl klass.Node.value)]
    | DefineMethod _ ->
	[]

let trans program =
  let tbl =
    methods_table program in
  let methods =
    methods_name_set   program in
    program +>  HList.concat_map (stmt_trans tbl methods)

let to_string =
  function
      Plain stmt ->
	Ast2.to_string_stmt stmt
    | DefineClass (name,super,attrs) ->
	Printf.sprintf "Class (%s,%s,%s)"
	  (Node.to_string id name)
	  (Node.to_string (fun (a,b) -> a^":"^b) super) @@
	  string_of_list @@ List.map (Node.to_string id) attrs
    | DefineMethod (f,(self,klass),args,body) ->
	let show =
	  Node.to_string id in
	Printf.sprintf "Metod (%s,((%s %s) %s),%s)" 
	  (show f) (show self) (show klass) 
	  (string_of_list (List.map show args)) (Ast2.to_string body)
