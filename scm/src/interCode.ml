(** intermediate code *)
type inter_code = {
  variables: Ast.qname list;
  methods: Ast.sname list;
  program: Ast.program
}

let empty = {
  variables = [];
  methods   = [];
  program   = []
}

let variable =
  function
      `Define ((`Public name),_) | `Class ((`Public name),_,_,_) ->
	[name]
    | `Expr _ | `Define _ | `Class _ ->
	[]

let method_ =
  function
      | `Class (_,_,_,methods) ->
	  List.map Tuple.T3.fst methods
    | `Expr _ | `Define _  ->
	[]

let of_program program = {
  variables = HList.concat_map variable program;
  methods   = HList.concat_map method_  program;
  program   = program
}

let output ch (program : inter_code) =
  output_value ch program

let input ch : inter_code =
  input_value ch
