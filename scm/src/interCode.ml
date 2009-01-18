open Base

type inter_code = {
  variables: (string * string) list;
  methods: string list;
  program: Ast.program
}

let empty = {
  variables = [];
  methods   = [];
  program   = []
}

let filter_variable =
  function
      `Define ((`Public name),_) | `Class ((`Public name),_,_,_) ->
	[Node.value name]
    | `Expr _ | `Define _ | `Class _ ->
	[]

let filter_method =
  function
      | `Class (_,_,_,methods) ->
	  List.map (Node.value $ Tuple.T3.fst) methods
      | `Expr _ | `Define _  ->
	  []

let of_program program = {
  variables = HList.concat_map filter_variable program;
  methods   = HList.concat_map filter_method   program;
  program   = program
}

let to_program {program=program} =
  program

let mem_variable v {variables=variables} =
  List.mem v variables

let mem_method m {methods=methods} =
  List.mem m methods

let output ch (icode : inter_code) =
  output_value ch icode

let input ch : inter_code =
  input_value ch

