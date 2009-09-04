(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "override.mlip"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "override.mlip"
type 'expr expr = 'expr Binding.expr

type 'expr method_ = ('expr Ast.method_) * [`Override] list
type ('expr,'stmt) stmt =
    [ `Define of Module.stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (Module.stmt_name,'expr method_) Ast.class_ ]



# 1 "../type/type.h" 1





# 11 "override.mlip" 2
val fold : ('a -> ([> 'b expr]) -> 'a) ->  ('a -> [> 'd expr] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr -> 'e
val lift : ('a -> 'b) -> [< ('a,'c) stmt ] -> [> ('b,'c) stmt ]
val fold_stmt : ('a -> [> ('b,'c) stmt] -> 'a) -> ('a -> [> ('b,'c) stmt] -> 'e) -> 'a -> ('b,'c) stmt -> 'e

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list

val of_binding : Binding.program -> program
