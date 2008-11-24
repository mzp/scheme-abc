type stmt = 
    Plain of Ast.stmt
  | DefineClass  of ident * Ast.name * ident list
  | DefineMethod of ident * (ident * ident) * ident list * Ast.expr
and attr = string Node.t
and ident = string Node.t

type program = stmt list

val trans : program -> Ast.program
val to_string : stmt -> string
