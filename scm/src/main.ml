open Base
open OptParse
open Node

(* error report *)
let chop name =
  try
    Filename.chop_extension @@ Filename.basename name
  with _ ->
    name

let file path ext =
  Printf.sprintf "%s%s" (chop @@ path) ext

let rec nth_line n ch =
  if n = 0 then
    input_line ch
  else begin
    ignore @@ input_line ch;
    nth_line (n-1) ch
  end

let error kind { value     = msg;
		 filename  = filename;
		 lineno    = lineno;
		 start_pos = a;
		 end_pos   = b } =
  let ch =
    open_in filename in
    Printf.eprintf "%s:%d: %s, %s\n" filename lineno kind msg;
    prerr_endline @@ nth_line lineno ch;
    for i = 0 to b - 1 do
      if i >= a then
	print_string "^"
      else
	print_string " "
    done;
    print_newline ();
    close_in ch

let error_report f =
  try
    f ()
  with
      Parser.Parsec.Syntax_error loc ->
	error "synatx error" loc;
	exit 1
    | Checker.Binding.Unbound_var ({Node.value=(ns,name)} as loc) ->
	let name =
	  String.concat "." @@ ns @ [name] in
	  error ("unbound variable") {loc with Node.value = name};
	  exit 1
    | Checker.Binding.Unbound_method loc ->
	error ("unbound method") loc;
	exit 1

let module_name path =
  Filename.basename @@ chop path

(* compile *)
let to_ast table input =
  input
  +> Node.of_file
  +> Parser.Main.parse table
  +> Checker.Main.check table

let find includes file =
  try
    let inc =
      List.find (fun inc -> Sys.file_exists @@ Filename.concat inc file)
	includes in
      Filename.concat inc file
  with Not_found ->
    failwith ("Not found: " ^ file)

let build ~extern ~includes ~inputs ~output =
  inputs
  +> List.map (find includes)
  +> List.fold_left begin
       fun table input ->
	 if Filename.check_suffix input ".ho" then
	   InterCode.input (module_name input) input table
	 else
	   InterCode.add (module_name input) (to_ast extern input) table
     end InterCode.empty
  +> (fun table -> table#to_ast)
  +> Codegen.Main.output (open_out_bin output)

let compile table input output =
  input
  +> to_ast table
  +> (fun program -> InterCode.add (module_name output) program table)
  +> InterCode.output (module_name output) (file output ".ho")

(* arguments *)
let get_option x =
   Option.get @@ x.Opt.option_get ()

let parse_arguments _ =
  let opt =
    OptParser.make ~version:Config.version () in
  let output =
    StdOpt.str_option ~default:"" ~metavar:"<output>" () in
  let compile_only =
    StdOpt.store_true () in
  let include_dir =
    StdOpt.str_option ~default:"" ~metavar:"<include_dir ...>" () in
  let _ =
    OptParser.add opt ~short_name:'o' ~long_name:"output"
      ~help:"Set output file name to <file>" output;
    OptParser.add opt ~short_name:'I'
      ~help:"Include directory list" include_dir;
    OptParser.add opt ~short_name:'c' ~long_name:"compile-only"
      ~help:"Compile only" compile_only in
  let inputs =
    OptParser.parse_argv opt in
  let includes =
    (Str.split (Str.regexp Config.path_sep) @@ get_option include_dir) in
    if inputs = [] then begin
      OptParser.usage opt ();
      exit 0
    end else if get_option compile_only then
      `CompileOnly {| include_dir = includes;
		      input = List.hd inputs;
		      output = file (List.hd inputs) ".ho"|}
    else
      `Link {| include_dir = includes;
	       inputs = inputs;
	       output = let o =
		          get_option output in
		          if o = "" then "a.abc" else o |}

let readdir path =
  Sys.readdir path
  +> Array.to_list
  +> List.map (fun s -> Filename.concat path s)

let input_dir table dir =
  dir
  +> readdir
  +> List.filter (flip Filename.check_suffix ".ho")
  +> List.fold_left
       (fun table input -> InterCode.input (module_name input) input table)
       table

let read_inter_code dirs =
  dirs
  +> List.fold_left input_dir InterCode.empty

let main () =
  match parse_arguments () with
      `CompileOnly o ->
	let table =
	  read_inter_code o#include_dir in
	  error_report begin fun () ->
	    compile table o#input o#output
	  end
    | `Link o ->
	let table =
	  read_inter_code o#include_dir in
	  error_report begin fun () ->
	    build ~extern:table ~inputs:o#inputs
	          ~includes:o#include_dir ~output:o#output
	  end

let _ =
  if not !Sys.interactive then
    main ()
