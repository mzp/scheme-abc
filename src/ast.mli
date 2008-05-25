type ast = 
    Method of string * ast
  | Call of string * ast list
  | String of string
  | Int of int
  | Add of ast * ast
  | Sub of ast * ast
  | Mul of ast * ast  
  | Div of ast * ast
  | Eq of ast * ast
  | Lt of ast * ast (* less than*)
  | Leq of ast * ast (* less than equals *)
  | Gt of ast * ast (* greater than *)
  | Geq of ast * ast (* greatr than equlas *)
  | If of ast * ast * ast
  | Let of (string*ast) list * ast
  | Var of string
  | Block of ast list

val generate_method : ast -> Asm.meth
val generate : ast -> Abc.abc
