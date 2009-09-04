open Base

type entry = {
  symbols : (string list * string) list;
  methods : string list;
  modules : string list list;
  program : Ast.program
}

let module_ name program : Ast.stmt' =
  `Module {Ast.module_name = Node.ghost name;
	   exports         = `All;
	   stmts           = program}

class t = object
  val entries = [] with reader

  method mem_symbol (ns,name) =
    let file, sym =
      match ns with
	  [] ->
	    "stub",([],name)
	| x::xs ->
	    x,(xs,name) in
      try
	let lazy {symbols=symbols} =
	  List.assoc file entries in
	  List.mem sym symbols
      with Not_found ->
        false

  method mem_method meth =
    List.exists (fun (_,lazy {methods=methods}) ->
		   List.mem meth methods) entries

  method mem_module name =
    match name with
	[] ->
	  failwith "empty list"
      | file::ns ->
	  try
	    let lazy {modules=modules} =
	      List.assoc file entries in
	      List.mem ns modules
	  with Not_found ->
            false

  method to_ast =
    List.map (fun (name,lazy {program=program}) -> module_ name program) entries

  method add name entry =
    {< entries = entries @ [name,entry] >}
end

(* program -> table *)
let empty =
  new t

let to_entry program= {
  symbols =
    program
    +> HList.concat_map Ast.public_symbols
    +> List.map Node.value;
  methods=
    program
    +> HList.concat_map Ast.public_methods
    +> List.map Node.value;
  modules =
    program
    +> HList.concat_map Ast.public_modules
    +> List.map Node.value;
  program =
    program
}

let chop_suffix name suffix =
  if Filename.check_suffix name suffix then
    Filename.chop_suffix name suffix
  else
    name

let version = 2

let add name program table =
  table#add name @@ (lazy (to_entry program))

let input name path table =
  let entry _ =
    open_in_with path begin fun ch ->
    let version' =
      input_value ch in
      if version' = version then
	let symbols = input_value ch in
	let methods = input_value ch in
	let modules = input_value ch in
	let program = input_value ch in
	  { symbols = symbols;
	    methods = methods;
	    modules = modules;
	    program = program }
      else
	failwith ("invalid format:"^path)
    end in
    table#add name (lazy (entry ()))

let output name path table =
  table#entries
  +> List.assoc name
  +> (fun (lazy entry) ->
	open_out_with path begin fun ch ->
	  output_value ch version;
	  output_value ch entry.symbols;
	  output_value ch entry.methods;
	  output_value ch entry.modules;
	  output_value ch entry.program
	end)
