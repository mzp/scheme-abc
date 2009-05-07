val generate : Binding.slot list -> Binding.program -> Abc.abc

(**{6 Debug only}*)
val generate_program : Binding.program -> Asm.instruction list
