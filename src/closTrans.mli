type stmt = 
    Plain of Ast.stmt
  | DefineClass  of string * Ast.name * attr list
  | DefineMethod of string * (string * string) * string list * Ast.expr
and attr = string

type program = stmt list

val trans : program -> Ast.program
val to_string : stmt -> string
