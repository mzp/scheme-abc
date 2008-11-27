exception Unbound_var of string Node.t
exception Unbound_class of (string*string) Node.t
exception Unbound_method of string Node.t

type method_ = Ast.ident
type stmt =
    [ `ExternalClass of Ast.name * method_ list
    | `External of Ast.ident
    | Ast.stmt]

val check : stmt list -> Ast.stmt list
