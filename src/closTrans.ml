open Base

type stmt = 
    Plain of Ast.stmt
  | DefineClass  of string * Ast.name * string list
  | DefineMethod of string * (string * string) * string list * Ast.expr

type program = stmt list

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

let classize program tbl =
  HList.concat_map 
    (function
	 Plain stmt ->
	   [stmt]
       | DefineClass (klass,super,_) ->
	   [Ast.Class (klass,super,Hashtbl.find_all tbl klass)]
       | DefineMethod _ ->
	   []) program

let trans program =
  classize program @@ methods_table program

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

