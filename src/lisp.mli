exception Syntax_error of string

val compile : char Node.t Stream.t -> ClosTrans.program
val compile_string : string -> ClosTrans.program
