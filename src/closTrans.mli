type stmt_term =
    [ ModuleTrans.stmt_term
    | `DefineClass  of Ast.sname * Ast.qname * Ast.attr list
    | `DefineMethod of Ast.sname * (Ast.sname * Ast.sname) *
	Ast.sname list * Ast.expr ]

type stmt =
    [ stmt_term
    | `Module of Ast.sname * Ast.sname list * stmt list ]

type program = stmt list

val trans : program -> ModuleTrans.program
