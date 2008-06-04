type expr = 
    Lambda of string list * expr
  | Call of string * expr list
  | String of string
  | Int of int
  | If of expr * expr * expr
  | Let of (string*expr) list * expr
  | Var of string
  | Block of expr list

(** statement has side-effect *)
type stmt = 
  | Define of string * expr
  | Expr of expr

type program = stmt list

val generate : program -> Abc.abc

val generate_method : program -> Asm.meth
