(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "binding.mlip"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "binding.mlip"
type scope = Scope of int | Global

type name = string list * string

type bind =
    Register of int
  | Slot of scope * int
  | Member of scope * name

type slot = name * int

type 'expr expr =
    [ 'expr Module.expr
    | `BindVar of bind Node.t]

type ('expr,'stmt) stmt =
    ('expr,'stmt) Module.stmt



# 1 "../type/type.h" 1





# 21 "binding.mlip" 2
val fold : ('a -> ([> 'b expr]) -> 'a) ->  ('a -> [> 'd expr] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr -> 'e
val lift : ('a -> 'b) -> [< ('a,'c) stmt ] -> [> ('b,'c) stmt ]
val fold_stmt : ('a -> [> ('b,'c) stmt] -> 'a) -> ('a -> [> ('b,'c) stmt] -> 'e) -> 'a -> ('b,'c) stmt -> 'e

type expr' =
    expr' expr

type stmt' =
    (expr',stmt') stmt

type program =
    stmt' list

val of_module : Module.program -> slot list * program
