type scope = Global | Scope of int
type bind = Register of int | Slot of scope * int | Member of scope * string


type 'expr expr_type =
    [ 'expr Ast.expr_type
    | `BindVar of bind Node.t]
type expr =
    expr expr_type
type 'expr stmt_type =
    [ 'expr Ast.stmt_type
    | `ReDefine of Ast.stmt_name * 'expr]

type stmt =
    expr stmt_type

type program =
    stmt list

val trans : Ast.program -> program
