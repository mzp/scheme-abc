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

type expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of qname
    | `Lambda  of sname list * expr
    | `Call    of expr list
    | `If      of expr * expr * expr
    | `Let     of (sname*expr) list * expr
    | `LetRec  of (sname*expr) list * expr
    | `Block   of expr list
    | `New     of qname * expr list
    | `Invoke  of expr   * sname * expr list (** (invoke <object> <method-name> <arg1> <arg2>...)*)
    | `SlotRef of expr * sname
    | `SlotSet of expr * sname * expr ]

(** statement has side-effect *)
type attr    = sname
type stmt_name  =
    [ `Public of qname
   | `Internal of qname]

type method_name =
    [ `Public of sname
    | `Static of sname ]

type 'expr method_type = {
  method_name : method_name;
  args : sname list;
  body : 'expr;
}

type 'expr class_type = {
  klass_name : stmt_name;
  super: qname;
  attrs: attr list;
  methods: 'expr method_type list
}

type 'expr stmt_type =
    [ `Define of stmt_name * 'expr
    | `Expr of 'expr
    | `Class of 'expr class_type ]

type method_ =
    expr method_type
type stmt =
    expr stmt_type

type program = stmt list

(** [map f e] applys f to all-sub expression of [e]. *)
val map : (expr -> expr) -> expr -> expr
val fold_up : ('a expr_type -> 'a) -> (expr -> 'a ) -> expr -> 'a

(**{6 Lift}*)
val lift_stmt : (expr->expr) -> stmt -> stmt
val lift_program : (expr->expr) -> program -> program

