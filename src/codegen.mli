val generate : VarResolve.slot list -> VarResolve.program -> Abc.abc

(**{6 Debug only}*)
val generate_program : VarResolve.program -> Asm.instruction list
