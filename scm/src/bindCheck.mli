exception Unbound_var of (string*string) Node.t
exception Forbidden_var of (string*string) Node.t
exception Unbound_method of string Node.t

val check : InterCode.table -> ModuleTrans.program -> ModuleTrans.program
val uncheck : ModuleTrans.program -> ModuleTrans.program

