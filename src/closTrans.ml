open Base

type clos_stmt = 
    Plain of Ast.stmt
  | DefineClass  of string * Ast.name * string list
  | DefineMethod of string * (string * string) * string list * Ast.expr

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
