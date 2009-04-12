
type 'a expr_type = 'a ModuleTrans.expr_type
type class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.attr list;
}

type 'expr method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: 'expr
}

type ('expr,'stmt) stmt_type =
    [ ('expr,'stmt) ModuleTrans.expr_stmt_type
    | ('expr,'stmt) ModuleTrans.module_stmt_type
    | `DefineClass  of class_
    | `DefineMethod of 'expr method_
    | `DefineStaticMethod of 'expr method_ ]

type expr =
    expr expr_type

type stmt =
    (expr,stmt) stmt_type

type program = stmt list

val trans : InterCode.table -> program -> ModuleTrans.program

val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e
val fold_stmt :   ('a -> [> ('b,'c) stmt_type ] -> 'a) -> ('a -> [> ('b,'d) stmt_type ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) stmt_type -> 'e
val lift :  ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) stmt_type ] -> [> ('b,'d) stmt_type]
