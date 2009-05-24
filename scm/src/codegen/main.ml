open Base


let generate program =
  program
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> curry Codegen.generate
  +> Abc.to_bytes
  +> Bytes.to_int_list

let output ch program =
  program
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> curry Codegen.generate
  +> Abc.to_bytes
  +> Bytes.output_bytes ch

