open Base

type method_ = Ast.ident * Ast.ident list 

type stmt =
    [ `ExternalClass of Ast.ident * Ast.name * Ast.attr list * method_ list
    | `External of Ast.ident
    | Ast.stmt]
