(** Abstract syntax tree *)

(** name := namespace * symbol *)
type name = string * string

(** A type of expression. Expression does not have side-effect. *)
type expr = 
    Int     of int
  | String  of string
  | Bool    of bool
  | Float   of float
  | Var     of string
  | Lambda  of string list * expr
  | Call    of expr list
  | If      of expr * expr * expr
  | Let     of (string*expr) list * expr
  | LetRec  of (string*expr) list * expr
  | Block   of expr list
  | New     of name * expr list
  | Invoke  of expr * string * expr list (** (. obj (f <arg1> <arg2> ..)) *)
  | SlotRef of expr * string             (** (slot-ref  <obj> <name>) *)
  | SlotSet of expr * string * expr      (** (slot-set! <obj> <name> <value>) *)

(** A type of statement. Statement has side-effect *)
type stmt = 
  | Define of string * expr
  | Expr of expr
  | Class of string * name * attr list * method_ list
and attr    = string
and method_ = string * string list * expr

(** A tyye of program. *)
type program = stmt list

(** [map f e] applys f to all-sub expression of [e]. *)
val map : (expr -> expr) -> expr -> expr

val to_string : expr -> string
val to_string_stmt : stmt -> string

(**{6 Lift}*)

val lift_stmt : (expr->expr) -> stmt -> stmt
val lift_program : (expr->expr) -> program -> program
