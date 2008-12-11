type stmt_term =
    [ ModuleTrans.stmt_term
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.qname) *
	Ast.sname list * Ast.expr ]
type stmt =
    [ stmt_term]

type program = stmt list

val trans : program -> BindCheck.program
