open Base

module A = Asm.Make(Instruction)

let save_first f (a,b) =
  (a,f a b)

let to_bytes program =
  program
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> Tuple.T2.map2 Override.of_binding
  +> save_first Codegen.generate
  +> curry A.assemble
  +> Abc.to_bytes

let generate program =
  program
  +> to_bytes
  +> Bytes.to_int_list

let output ch program =
  program
  +> to_bytes
  +> Bytes.output_bytes ch
