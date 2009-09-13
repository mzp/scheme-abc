open Base

let to_multiname (ns,name) =
  `QName (`Namespace (String.concat "." ns),name)

let to_bytes program =
  program
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> (fun (a,b) ->
	List.map to_multiname a,
	Codegen.generate @@ Override.of_binding b)
  +> curry Abc.compile
  +> Abc.asm

let generate program =
  program
  +> to_bytes
  +> Bytes.to_int_list

let output ch program =
  program
  +> to_bytes
  +> Bytes.output_bytes ch

