exception Unbound_var of (string*string) Node.t
exception Unbound_class of (string*string) Node.t
exception Unbound_method of string Node.t

type stmt =
    [ `ExternalClass of Ast.qname * Ast.sname list
    | `External of Ast.qname
    | Ast.stmt]

type program = stmt list

val check : stmt list -> Ast.program
val uncheck : stmt list -> Ast.program



