(** Module transformer: flatten module *)

(**{6 Types}*)
type method_ =
  Ast.sname * Ast.sname list * Ast.expr

type exports =
    All
  | Restrict of Ast.sname list

type 'stmt stmt_type =
    [ `Class  of Ast.sname * Ast.qname * Ast.attr list * method_ list
    | `Define of Ast.sname * Ast.expr
    | `Expr   of Ast.expr
    | `ExternalClass of Ast.sname * Ast.sname list
    | `External of Ast.sname
    | `Module of Ast.sname * exports * 'stmt list ]

type stmt =
    stmt stmt_type

type program = stmt list

(**{6 Trans}*)
val trans : program -> BindCheck.program

(**{6 Lift}*)
val lift  : (Ast.expr -> Ast.expr) -> stmt -> stmt

