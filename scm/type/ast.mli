(* CAUTION: this is a generated file.  If you edit it, all changes will be lost! *)
# 1 "ast.mlip"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "ast.mlip"
open Base

type qname = (string list * string) Node.t
type sname = string Node.t

type 'expr expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
    | `Array   of 'expr list
    | `Lambda  of sname list * 'expr
    | `Call    of 'expr list
    | `If      of 'expr * 'expr * 'expr
    | `Let     of (sname*'expr) list * 'expr
    | `LetRec  of (sname*'expr) list * 'expr
    | `Block   of 'expr list
    | `New     of qname * 'expr list
    | `Invoke  of 'expr   * sname * 'expr list
    | `SlotRef of 'expr * sname
    | `SlotSet of 'expr * sname * 'expr ]

type 'stmt module_ = {
  module_name : sname;
  exports : [`All | `Only of sname list];
  stmts   : 'stmt list
}
type ('expr,'stmt) module_stmt =
    [ `Module of 'stmt module_
    | `Open   of string list Node.t ]

type 'expr method_ = {
  method_name : [`Public of sname | `Static of sname];
  args : sname list;
  body : 'expr;
}

type ('name,'method_) class_ = {
  class_name : 'name;
  super: qname;
  attrs: sname list;
  methods: 'method_ list
}

type ('expr,'stmt) expr_stmt =
    [ `Define of sname * 'expr
    | `Expr   of 'expr ]

type ('expr,'stmt) class_stmt =
    [ `Class  of (sname, 'expr method_) class_ ]

type ('expr,'stmt) stmt =
    [ ('expr,'stmt) class_stmt
    | ('expr,'stmt) expr_stmt
    | ('expr,'stmt) module_stmt ]

type expr' =
    expr' expr
type stmt' =
    (expr',stmt') stmt
type program =
    stmt' list


# 1 "type.h" 1





# 66 "ast.mlip" 2
val fold : ('a -> ([> 'b expr]) -> 'a) ->  ('a -> [> 'd expr] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr -> 'e

val fold_expr_stmt : ('a -> [> ('b,'c) expr_stmt] -> 'a) -> ('a -> [> ('b,'c) expr_stmt] -> 'e) -> 'a -> ('b,'c) expr_stmt -> 'e
val fold_module_stmt : ('a -> [> ('b,'c) module_stmt ] -> 'a) -> ('a -> [> ('b,'d) module_stmt ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) module_stmt -> 'e
val fold_class_stmt : ('a -> [> ('b,'c) class_stmt] -> 'a) -> ('a -> [> ('b,'c) class_stmt] -> 'e) -> 'a -> ('b,'c) class_stmt -> 'e
val fold_stmt : ('a -> [> ('b,'c) stmt ] -> 'a) -> ('a -> [> ('b,'d) stmt ] -> 'e) ->  ('a -> 'c -> 'd) -> 'a -> ('b, 'c) stmt -> 'e

val lift_expr : ('a -> 'b) -> [< ('a,'c) expr_stmt ] -> [> ('b,'c) expr_stmt ]
val lift_module :  ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) module_stmt ] -> [> ('b,'d) module_stmt ]
val lift_class :  ('a -> 'b) -> [< ('a,'c) class_stmt ] -> [> ('b,'c) class_stmt ]
val lift : ('a -> 'b) ->  ('c -> 'd) ->  [< ('a,'c) stmt ] -> [> ('b,'d) stmt ]

(* combinator *)
val fix_fold : ('a -> 'b -> ('c -> 'd -> 'e) -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val fix_lift : ('a -> ('b -> 'c) -> 'b -> 'c) -> 'a -> 'b -> 'c
val map : (('a -> 'b -> 'b) -> ('c -> 'd -> 'e) -> ('f -> 'f -> 'g) -> 'f -> 'f -> 'g) -> ('d -> 'e) -> 'f -> 'g

val public_symbols : stmt' -> qname list
val public_methods : stmt' -> sname list
val public_modules : stmt' -> string list Node.t list
