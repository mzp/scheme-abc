type ast = 
    Method of string * ast list 
  | Call of string * ast list
  | String of string

val generate_method : ast -> Asm.meth list
val generate : ast list -> Abc.abc
