type class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
}

type method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: Ast.expr
}

type 'stmt stmt_type =
    [ 'stmt BindCheck.stmt_type
    | `DefineClass  of class_
    | `DefineMethod of method_
    | `DefineStaticMethod of method_ ]

type stmt =
    stmt stmt_type

type program = stmt list

val trans : InterCode.table -> program -> BindCheck.program
