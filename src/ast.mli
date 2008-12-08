(** Abstract syntax tree *)

(** name := namespace * symbol *)
type name = (string * string) Node.t

type ident = string Node.t

(** expression has no side-effect. *)
type expr =
    [ `Int     of int Node.t
    | `String  of string Node.t
    | `Bool    of bool Node.t
    | `Float   of float Node.t
    | `Var     of name
    | `Lambda  of ident list * expr
    | `Call    of expr list
    | `If      of expr * expr * expr
    | `Let     of (ident*expr) list * expr
    | `LetRec  of (ident*expr) list * expr
    | `Block   of expr list
    | `New     of name * expr list
    | `Invoke  of expr   * ident * expr list (** (invoke <object> <method-name> <arg1> <arg2>...)*)
    | `SlotRef of expr * ident
    | `SlotSet of expr * ident * expr ]

type attr    = ident
type method_ = ident * ident list * expr

(** statement has side-effect *)
type stmt =
    [ `Define of name * expr
    | `Expr of expr
    | `Class of name * name * attr list * method_ list ]

type program = stmt list


(** [map f e] applys f to all-sub expression of [e]. *)
val map : (expr -> expr) -> expr -> expr

val to_string : expr -> string
val to_string_stmt : stmt -> string

(**{6 Lift}*)

val lift_stmt : (expr->expr) -> stmt -> stmt
val lift_program : (expr->expr) -> program -> program

