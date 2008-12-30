exception Unbound_var of (string*string) Node.t
exception Unbound_method of string Node.t

type 'stmt stmt_type =
    [ `ExternalClass of Ast.sname * Ast.sname list
    | `External of Ast.sname
    | 'stmt ModuleTrans.stmt_type]

type stmt =
    stmt stmt_type

type program = stmt list

val check : stmt list -> ModuleTrans.program
val uncheck : stmt list -> ModuleTrans.program

val lift : (Ast.expr -> Ast.expr) -> stmt -> stmt
