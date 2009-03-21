(** Module transformer: flatten module *)

(**{6 Types}*)
type exports =
    All
  | Restrict of Ast.sname list

type klass_type = {
  klass_name : Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
  methods: Ast.method_ list
}

type 'stmt module_type = {
  module_name : Ast.sname;
  exports : exports;
  stmts   : 'stmt list
}

type 'stmt stmt_type =
    [ `Class  of klass_type
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

