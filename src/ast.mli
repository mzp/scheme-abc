(** Abstract syntax tree *)

(** name := namespace * symbol *)
type qname = (string * string) Node.t
type sname = string Node.t

(** expression has no side-effect. *)
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

type attr    = sname
type method_ = sname * sname list * expr

(** statement has side-effect *)
type stmt =
    [ `Define of qname * expr
    | `Expr of expr
    | `Class of qname * qname * attr list * method_ list ]

type program = stmt list


(** [map f e] applys f to all-sub expression of [e]. *)
val map : (expr -> expr) -> expr -> expr

val string_of_qname : qname -> string
val to_string : expr -> string
val to_string_stmt : stmt -> string

(**{6 Lift}*)

val lift_stmt : (expr->expr) -> stmt -> stmt
val lift_program : (expr->expr) -> program -> program

