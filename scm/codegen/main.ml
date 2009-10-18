open Base

let to_multiname (ns,name) =
  `QName (`Namespace (String.concat "." ns),name)

let generate program =
  program
  +> Module.of_ast
  +> ClosureTrans.trans
  +> Binding.of_module
  +> (fun (a,b) ->
	List.map to_multiname a,
	Codegen.generate @@ Override.of_binding b)
  +> curry Swflib.Abc.compile
  +> Swflib.Abc.write

let output ch program =
  program
  +> generate
  +> List.iter (output_byte ch)
