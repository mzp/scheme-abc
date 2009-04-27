open Base

type entry = {
  symbols: (string list * string) list;
  methods: string list;
}
type table = (string * entry) list

let version = 2

let empty = []

let to_entry program= {
  symbols =
    program
    +> HList.concat_map Ast.public_symbols
    +> List.map Node.value;
  methods=
    program
    +> HList.concat_map Ast.public_methods
    +> List.map Node.value
}

let root_module =
  function
      [] -> ""
    | x::_ -> x

let sub_module =
  function
      [] -> []
    | _::xs -> xs

let mem_variable (m,name) table =
  match maybe (List.assoc @@ root_module m) table with
      None ->
	false
    | Some {symbols=symbols} ->
	List.mem (sub_module m,name) symbols

let mem_method m table =
  table +> List.exists
    (fun (_,{methods=methods}) ->
       List.mem m methods)

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

let write path program =
  let {methods=methods; symbols=symbols} =
    to_entry program in
  open_out_with (filename path "ho") begin fun ch ->
    output_value ch version;
    output_value ch symbols;
    output_value ch methods;
    output_value ch program
  end

let module_name path =
  Filename.basename @@
    Filename.chop_suffix path ".ho"

let load_program path : Ast.program =
  open_in_with path begin fun ch ->
    let version' =
      input_value ch in
      if version' = version then begin
	ignore @@ input_value ch;
	ignore @@ input_value ch;
	[`Module {Ast.module_name = Node.ghost @@ module_name path;
		  exports                 = `All;
		  stmts                   = input_value ch}]
      end else
	failwith ("invalid format:"^path)
  end

let readdir path =
  Sys.readdir path +>
    Array.to_list +>
    List.map (fun s -> Filename.concat path s)

let add_program table name program =
  (name,to_entry program)::table

let add_dir table dir =
  dir
  +> readdir
  +> List.filter (flip Filename.check_suffix ".ho")
  +> List.map (fun path->
		 open_in_with path begin fun ch ->
		   let version' =
		     input_value ch in
		     if version' = version then begin
		       let symbols =
			 input_value ch in
		       let methods =
			 input_value ch in
		       (module_name path,{
			  symbols = symbols;
			  methods = methods
			})
		     end else
		       failwith ("invalid format: "^path)
		 end) @ table
