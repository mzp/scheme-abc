open Base

type inter_code = {
  variables: (string * string) list;
  methods: string list;
  program: Ast.program
}

type table = (string*inter_code) list

let empty = []

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

let add table name icode =
  (name,icode)::table

let add_file _ =
  failwith "not yeti"

let mem_variable _ _ =
  true

let mem_method _ _ =
  true

let output ch (icode : inter_code) =
  output_value ch icode

let input ch : inter_code =
  input_value ch

