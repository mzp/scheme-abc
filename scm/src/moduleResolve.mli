type 'a expr_type = 'a ModuleTrans.expr_type
type ('a,'b) stmt_type = [
  ('a,'b) ModuleTrans.stmt_type
| `Open of Ast.sname
]

val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e
val fold_stmt :   ('a -> [> ('b,'c) stmt_type ] -> 'a) -> ('a -> [> ('b,'d) stmt_type ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) stmt_type -> 'e
val lift :  ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) stmt_type ] -> [> ('b,'d) stmt_type]

val trans : 'a -> 'a
