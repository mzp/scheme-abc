type stmt = 
    Plain of Ast.stmt
  | DefineClass  of string * Ast.name * string list
  | DefineMethod of string * (string * string) * string list * Ast.expr

type program = stmt list

val trans : program -> Ast.program
