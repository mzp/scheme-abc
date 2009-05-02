(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "module.mlip"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "module.mlip"
type 'expr expr = 'expr Ast.expr

type stmt_name  =
    [ `Public of Ast.qname
    | `Internal of Ast.qname]

type ('expr,'stmt) stmt =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (stmt_name,'expr) Ast.class_ ]


# 1 "../type/type.h" 1





# 13 "module.mlip" 2
val fold : ('a -> ([> 'b expr]) -> 'a) ->  ('a -> [> 'd expr] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr -> 'e
val lift : ('a -> 'b) -> [< ('a,'c) stmt ] -> [> ('b,'c) stmt ]
val fold_stmt : ('a -> [> ('b,'c) stmt] -> 'a) -> ('a -> [> ('b,'c) stmt] -> 'e) -> 'a -> ('b,'c) stmt -> 'e

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list

val trans : Ast.program -> program

