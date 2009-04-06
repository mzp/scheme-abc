(** Module transformer: flatten module *)

(**{6 Types}*)
type 'stmt module_type = {
  module_name : Ast.sname;
  exports : [`All | `Only of Ast.sname list];
  stmts   : 'stmt list
}

type 'stmt stmt_type =
    [ `Class  of (Ast.sname,Ast.expr) Ast.class_type
    | `Define of Ast.sname * Ast.expr
    | `Expr   of Ast.expr
    | `Module of 'stmt module_type ]

type stmt =
    stmt stmt_type

type program = stmt list

(**{6 Trans}*)
val trans : program -> Ast.program

(**{6 Lift}*)
val lift  : (Ast.expr -> Ast.expr) -> stmt -> stmt

