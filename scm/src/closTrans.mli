type 'stmt stmt_type =
    [ 'stmt BindCheck.stmt_type
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.sname) *
	Ast.sname list * Ast.expr ]

type stmt =
    stmt stmt_type

type program = stmt list

val trans : InterCode.table -> program -> BindCheck.program
