exception Syntax_error of string * unit Node.t

val compile : char Node.t Stream.t -> ClosTrans.program
val compile_string : string -> ClosTrans.program
