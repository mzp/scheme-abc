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
	    "std",([""],name)
	| x::xs ->
	    x,(xs,name) in
    let lazy {symbols=symbols} =
      List.assoc file entries in
      List.mem sym symbols

  method mem_method meth =
    List.exists (fun (_,lazy {methods=methods}) -> List.mem meth methods) entries

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

let add name program table =
  table#add name @@ (lazy (to_entry program))

let output =
  undefined

let version = 1

let module_name path =
  Filename.basename @@
    Filename.chop_suffix path ".ho"

let input path table =
  let entry =
    open_in_with path begin fun ch ->
    let version' =
      input_value ch in
      if version' = version then
	lazy {
	  symbols = input_value ch;
	  methods = input_value ch;
	  program = input_value ch;
	}
      else
	failwith ("invalid format:"^path)
    end in
    table#add (module_name path) entry

let readdir path =
  Sys.readdir path +>
    Array.to_list +>
    List.map (fun s -> Filename.concat path s)

let input_dir dir table =
  dir
  +> readdir
  +> List.filter (flip Filename.check_suffix ".ho")
  +> List.fold_left (flip input) table

let suffix x =
  let regexp =
    Str.regexp ".*\\.\\(.*\\)$" in
    if Str.string_match regexp x 0 then
      Str.matched_group 1 x
    else
      invalid_arg "no suffix"

let filename name s =
  Printf.sprintf "%s%s"
    (Filename.chop_suffix name (suffix name))
    s

let output path table =
  table#entries
  +> List.iter (fun (path,lazy entry) ->
       open_out_with (filename path "ho") begin fun ch ->
	 output_value ch version;
	 output_value ch entry.symbols;
	 output_value ch entry.methods;
	 output_value ch entry.program
       end)
