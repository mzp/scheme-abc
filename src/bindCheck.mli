type method_ = Ast.ident
type stmt =
    [ `ExternalClass of Ast.name * method_ list
    | `External of Ast.ident
    | Ast.stmt]

val trans : stmt list -> (Ast.stmt list, string Node.t) Base.either
