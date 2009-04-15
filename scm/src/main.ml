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

let rec extract_line n ch =
  if n = 0 then
    input_line ch
  else begin
    ignore @@ input_line ch;
    extract_line (n-1) ch
  end

let error kind { value     = msg;
		 filename  = filename;
		 lineno    = lineno;
		 start_pos = a;
		 end_pos   = b } =
  let ch =
    open_in filename in
    Printf.eprintf "%s:%d: %s, %s\n" filename lineno kind msg;
    prerr_endline @@ extract_line lineno ch;
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
      Parsec.Syntax_error loc ->
	error "synatx error" loc;
	exit 1
    | BindCheck.Unbound_var ({Node.value=(ns,name)} as loc) ->
	let name =
	  if ns ="" then
	    name
	  else
	    ns ^ "." ^ name in
	  error ("unbound variable") {loc with Node.value = name};
	  exit 1
    | BindCheck.Unbound_method loc ->
	error ("unbound method") loc;
	exit 1

(* compile *)
let to_ast table path =
  if Filename.check_suffix path ".ho" then
    InterCode.load_program path
  else
    path
    +> Node.of_file
    +> Lisp.compile
    +> ClosTrans.trans table
    +> BindCheck.check table

let to_bytes ast =
  ast
  +> ModuleNameTrans.trans
  +> ModuleTrans.trans
  +> ClosureTrans.trans
  +> VarResolve.trans
  +> curry Codegen.generate
  +> Abc.to_bytes

let output_ast path ast =
  InterCode.write path ast

let output_bytes path bytes =
  open_out_with path begin fun ch ->
    Bytes.output_bytes ch bytes
  end

let build table inputs output =
  inputs
  +> List.map (to_ast table)
  +> HList.fold_left1 (@)
  +> to_bytes
  +> output_bytes output

let compile table input output =
  input
  +> to_ast table
  +> output_ast output

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
      `CompileOnly {|
	  include_dir = includes;
	  input       = List.hd inputs;
	  output      = file (List.hd inputs) ".ho"
      |}
    else
      `Link {|
	  include_dir = includes;
	  inputs      = inputs;
	  output      =
	    let o =
	      get_option output in
	      if o = "" then "a.abc" else o
      |}

let read_inter_code files =
  files +>
    List.fold_left InterCode.add_dir InterCode.empty

let main () =
  error_report begin fun () ->
    match parse_arguments () with
	`CompileOnly o ->
	  let table =
	    read_inter_code o#include_dir in
	    compile table o#input o#output
      | `Link o ->
	  let table =
	    read_inter_code o#include_dir in
	    build table o#inputs o#output
  end

let _ =
  if not !Sys.interactive then
    main ()
