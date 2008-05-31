type expr = 
    Lambda of string list * expr
  | Call of string * expr list
  | String of string
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr  
  | Div of expr * expr
  | Eq of expr * expr
  | Lt of expr * expr
  | Leq of expr * expr
  | Gt of expr * expr
  | Geq of expr * expr
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
