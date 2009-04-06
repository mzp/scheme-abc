(** Module transformer: flatten module *)

(**{6 Abstract types}*)
type 'stmt module_type = {
  module_name : Ast.sname;
  exports : [`All | `Only of Ast.sname list];
  stmts   : 'stmt list
}
type 'a expr_type = 'a Ast.expr_type

type ('expr,'stmt) stmt_type =
    [ `Class  of (Ast.sname, 'expr) Ast.class_type
    | `Define of Ast.sname * 'expr
    | `Expr   of 'expr
    | `Module of 'stmt module_type ]

val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e

val fold_stmt :   ('a -> [> ('b,'c) stmt_type ] -> 'a) -> ('a -> [> ('b,'d) stmt_type ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) stmt_type -> 'e

val lift :  ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) stmt_type ] -> ('b,'d) stmt_type

(**{6 Concreate type}*)
type expr    = expr expr_type
type stmt    = (expr,stmt) stmt_type
type program = stmt list
val trans : program -> Ast.program
