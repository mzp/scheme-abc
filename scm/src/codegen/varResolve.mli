(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "varResolve.mlip"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "varResolve.mlip"
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


# 1 "../type/type.h" 1





# 20 "varResolve.mlip" 2
val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e
val lift : LIFT_NOREC_1(stmt_type)
val fold_stmt : FOLD_STMT_NOREC_1(stmt_type)

type expr =
    expr expr_type

type stmt =
    expr stmt_type

type program =
    stmt list

val trans : Ast.program -> slot list * program
