type ast = 
    Method of string * ast list 
  | Call of string * ast list
  | String of string

val generate_expr : ast -> (Asm.meth list,Asm.instruction list) Base.either
val generate : ast list -> Abc.abc
