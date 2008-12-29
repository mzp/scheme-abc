val generate_program : VarResolve.program -> Asm.instruction list
val generate_script :  VarResolve.slot list  -> VarResolve.program -> Asm.meth
val generate : VarResolve.slot list -> VarResolve.program -> Abc.abc
