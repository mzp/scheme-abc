open Base

type inter_code = {
  variables: (string * string) list;
  methods: string list;
  program: Ast.program
}

type table = (string*inter_code lazy_t) list

let empty = []

let filter_variable =
  function
      `Define ((`Public name),_) | `Class {Ast.klass_name=`Public name} ->
	[Node.value name]
    | `Expr _ | `Define _ | `Class _ ->
	[]

let filter_method =
  function
      | `Class {Ast.methods=methods} ->
	  List.map (function {Ast.method_name=`Public m} |
			{Ast.method_name=`Static m} ->
			  Node.value m)
	    methods
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
  (name,lazy icode)::table

let output ch (icode : inter_code) =
  output_value ch icode

let input ch : inter_code =
  input_value ch

let add_file table path =
  let name =
    Filename.chop_suffix (Filename.basename path) ".ho" in
  let read () =
    let ch =
      open_in_bin path in
    let icode =
      input ch in
      close_in ch;
      icode in
    (name,lazy (read ()))::table

let root_module name =
  try
    let i =
      String.index name '.' in
      String.sub name 0 i
  with Not_found ->
    name

let sub_module name =
  try
    let i =
      String.index name '.' in
      String.sub name (i + 1) (String.length name - i - 1)
  with Not_found ->
    ""

let mem_variable (m,name) table =
  match maybe (List.assoc @@ root_module m) table with
      None ->
	false
    | Some icode -> (* i want to use lazy pattern *)
	let {variables=variables} =
	  !$ icode in
	  List.mem (sub_module m,name) variables

let mem_method m table =
  table +> List.exists
    (fun (_,icode) ->
       let {methods=methods} =
	 !$ icode in
	 List.mem m methods)
