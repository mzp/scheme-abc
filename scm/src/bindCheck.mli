exception Unbound_var of (string*string) Node.t
exception Forbidden_var of (string*string) Node.t
exception Unbound_method of string Node.t

type 'a stmt_type = 'a ModuleTrans.stmt_type
type stmt    = stmt stmt_type
type program = stmt list

val check : InterCode.table -> stmt list -> ModuleTrans.program
val uncheck : stmt list -> ModuleTrans.program

val lift : (Ast.expr -> Ast.expr) -> stmt -> stmt
