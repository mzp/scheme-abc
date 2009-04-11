type scope = Scope of int | Global

type name = string * string

type bind =
    Register of int
  | Slot of scope * int
  | Member of scope * name

type slot = name * int

type 'expr expr_type =
    [ 'expr Ast.expr_type
    | `BindVar of bind Node.t]
type 'expr stmt_type =
    [ 'expr Ast.stmt_type
    | `ReDefine of Ast.stmt_name * int * 'expr]

val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e
val lift :  ('a -> 'b) -> [< 'a stmt_type ] -> [> 'b stmt_type]
val fold_stmt : ('a -> [> 'b stmt_type] -> 'a) -> ('a -> [> 'b stmt_type] -> 'e) -> 'a -> 'b stmt_type -> 'e

type expr =
    expr expr_type

type stmt =
    expr stmt_type

type program =
    stmt list

val trans : Ast.program -> slot list * program
