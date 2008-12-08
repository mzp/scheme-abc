exception Unbound_var of (string*string) Node.t
exception Unbound_class of (string*string) Node.t
exception Unbound_method of string Node.t

type method_ = Ast.ident

type stmt =
    [ `ExternalClass of Ast.name * method_ list
    | `External of Ast.name
    | Ast.stmt]

type program = stmt list

val check : stmt list -> Ast.program
val uncheck : stmt list -> Ast.program
val to_string_stmt : stmt -> string


