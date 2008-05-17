type ast = 
    Method of string * ast list 
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


val generate_method : ast -> Asm.meth list
val generate : ast list -> Abc.abc
