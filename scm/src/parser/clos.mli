(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "clos.mlip"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "clos.mlip"
open Type

type 'a expr = 'a Ast.expr
type class_  = {
  class_name: Ast.sname;
  super: Ast.qname;
  attrs: Ast.sname list;
}

type 'expr method_ = {
  method_name: Ast.sname;
  to_class:    Ast.sname;
  args: Ast.sname list;
  body: 'expr
}

type ('expr,'stmt) stmt =
    [ ('expr,'stmt) Ast.expr_stmt
    | ('expr,'stmt) Ast.module_stmt
    | `DefineClass  of class_
    | `DefineMethod of 'expr method_
    | `DefineStaticMethod of 'expr method_ ]

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program = stmt' list


# 1 "../type/type.h" 1





# 31 "clos.mlip" 2
val fold : ('a -> ([> 'b expr]) -> 'a) ->  ('a -> [> 'd expr] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr -> 'e
val fold_stmt : ('a -> [> ('b,'c) stmt ] -> 'a) -> ('a -> [> ('b,'d) stmt ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) stmt -> 'e
val lift : ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) stmt ] -> [> ('b,'d) stmt ]

val to_ast : string PSet.t -> program -> Type.Ast.program
