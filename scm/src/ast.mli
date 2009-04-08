(** Abstract syntax tree *)

(** name := namespace * symbol *)
type qname = (string * string) Node.t
type sname = string Node.t

(** expression has no side-effect. *)
type 'expr expr_type =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
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

(** statement has side-effect *)
type method_name =
    [ `Public of sname
    | `Static of sname ]
type 'expr method_type = {
  method_name : method_name;
  args : sname list;
  body : 'expr;
}

type stmt_name  =
    [ `Public of qname
    | `Internal of qname]
type attr    =
    sname
type ('name,'expr) class_type = {
  class_name : 'name;
  super: qname;
  attrs: attr list;
  methods: 'expr method_type list
}

type 'expr stmt_type =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of (stmt_name,'expr) class_type ]

val fold : ('a -> ([> 'b expr_type]) -> 'a) ->  ('a -> [> 'd expr_type] -> 'e) -> ('a -> 'b -> 'd) -> 'a -> 'b expr_type -> 'e
val lift :  ('a -> 'b) -> [< 'a stmt_type ] -> [> 'b stmt_type]
val fold_stmt : ('a -> [> 'b stmt_type] -> 'a) -> ('a -> [> 'b stmt_type] -> 'e) -> 'a -> 'b stmt_type -> 'e

(** {6 concreate type} *)
type expr =
    expr expr_type

type method_ =
    expr method_type
type stmt =
    expr stmt_type
type program =
    stmt list

val map : (expr -> expr) -> expr -> expr


