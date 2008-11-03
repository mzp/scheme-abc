open Base

type stmt = 
    Plain of Ast.stmt
  | DefineClass  of string * Ast.name * string list
  | DefineMethod of string * (string * string) * string list * Ast.expr
and attr = string

type program = stmt list

module Set = Core.Std.Set
type 'a set = 'a Set.t

let set_of_list xs =
  List.fold_left (flip Set.add) Set.empty xs

(*
  Features:
  - convert DefineClass & DefineMethod to Ast.Class
  - convert Ast.Call to Ast.Invoke
*)

let methods_table program =
  let tbl =
    Hashtbl.create 16 in
    program +> List.iter
      (function
           DefineMethod (name,(self,klass),args,body) ->
	     Hashtbl.add tbl klass (name,self::args,body)
	 | _ ->
	     ());
    tbl

let methods_set program =
  set_of_list @@ HList.concat_map 
    (function
         DefineMethod (name,_,_,_) ->
	   [name]
       | _ ->
	   []) program

let expr_trans set =
  function
      Ast.Call ((Ast.Var f)::obj::args) when Set.mem f set ->
	Ast.Invoke (obj,f,args)
    | e ->
	e

let stmt_trans tbl set =
  function
      Plain stmt ->
	[Ast.lift_stmt (expr_trans set) stmt]
    | DefineClass (klass,super,attrs) ->
	[Ast.Class (klass,super,attrs,Hashtbl.find_all tbl klass)]
    | DefineMethod _ ->
	[]

let trans program =
  let tbl =
    methods_table program in
  let methods =
    methods_set   program in
    program +>  HList.concat_map (stmt_trans tbl methods)

let to_string =
  function
      Plain stmt ->
	Ast.to_string_stmt stmt
    | DefineClass (name,(ns,super),attrs) ->
	Printf.sprintf "Class (%s,%s::%s,%s)"
	  name
	  ns super @@
	  string_of_list attrs
    | DefineMethod (f,(self,klass),args,body) ->
	Printf.sprintf "Metod (%s,((%s %s) %s),%s)" 
	  f self klass (string_of_list args) (Ast.to_string body)

