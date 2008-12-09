type stmt =
    [ BindCheck.stmt
    | `DefineClass  of Ast.qname * Ast.qname * ident list
    | `DefineMethod of ident * (ident * Ast.qname) * ident list * Ast.expr]
and attr = string Node.t
and ident = string Node.t

type program = stmt list

val trans : program -> BindCheck.program
val to_string : stmt -> string
