open Base

let to_bytes program =
  program
  +> Scope.trans
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> curry Codegen.generate
  +> Abc.to_bytes

let generate program =
  program
  +> to_bytes
  +> Bytes.to_int_list

let output ch program =
  program
  +> to_bytes
  +> Bytes.output_bytes ch

