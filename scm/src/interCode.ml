open Base

type entry = {
  symbols: (string list * string) list;
  methods: string list;
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
	    "std",([],name)
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

  method to_ast =
    List.map (fun (name,lazy {program=program}) -> module_ name program) entries

  method add name entry =
    {<entries = (name,entry)::entries>}
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
  program =
    program
}

let chop_suffix name suffix =
  if Filename.check_suffix name suffix then
    Filename.chop_suffix name suffix
  else
    name

let version = 1

let module_name path =
  Filename.basename @@
    chop_suffix path ".ho"

let filename name =
  Printf.sprintf "%s.ho" name

let add name program table =
  table#add (module_name name) @@ (lazy (to_entry program))

let input path table =
  let entry _ =
    open_in_with path begin fun ch ->
    let version' =
      input_value ch in
      if version' = version then
	let symbols = input_value ch in
	let methods = input_value ch in
	let program = input_value ch in
	  { symbols = symbols;
	    methods = methods;
	    program = program }
      else
	failwith ("invalid format:"^path)
    end in
    table#add (module_name path) (lazy (entry ()))

let readdir path =
  Sys.readdir path
  +> Array.to_list
  +> List.map (fun s -> Filename.concat path s)

let input_dir dir table =
  dir
  +> readdir
  +> List.filter (flip Filename.check_suffix ".ho")
  +> List.fold_left (flip input) table

let output path table =
  table#entries
  +> List.iter (fun (name,lazy entry) ->
       open_out_with (Filename.concat path (filename name)) begin fun ch ->
	 output_value ch version;
	 output_value ch entry.symbols;
	 output_value ch entry.methods;
	 output_value ch entry.program
       end)
