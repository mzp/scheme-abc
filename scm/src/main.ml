open Base
open OptParse
open Node

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

let to_ast table stream =
  error_report
    (fun () ->
       let ast =
	 ClosTrans.trans @@ Lisp.compile stream in
	 ClosureTrans.trans @@
	   ModuleTrans.trans @@ BindCheck.check table ast)

let to_bytes ast =
  let abc =
    curry Codegen.generate @@ VarResolve.trans ast in
    Abc.to_bytes abc

let open_out_with path f =
  let ch =
    open_out_bin path in
  let s =
    f ch in
    close_out ch;
    s

let open_in_with path f =
  let ch =
    open_in_bin path in
  let s =
    f ch in
    close_in ch;
    s

let output_bytes path bytes =
  open_out_with path
    (fun ch ->
       Bytes.output_bytes ch bytes)

let output_ast path ast =
  open_out_with path
    (fun ch ->
       InterCode.output ch @@ InterCode.of_program ast)


let build table inputs output =
  let asts =
    inputs +> List.map
      (fun input ->
	 if Filename.check_suffix input ".ho" then
	   open_in_with input (InterCode.to_program $ InterCode.input)
	 else
	   to_ast table @@ Node.of_file input) in
    output_bytes output @@ error_report
      (fun () ->
	 to_bytes @@ HList.fold_left1 (@) asts)

let compile table output input =
  output_ast output @@ error_report
    (fun () ->
       to_ast table @@ Node.of_file input)

let get_option x =
   Option.get @@ x.Opt.option_get ()

let main () =
  let opt =
    OptParser.make ~version:Config.version () in
  let output =
    StdOpt.str_option ~default:"" ~metavar:"<output>" () in
  let compile_only =
    StdOpt.store_true () in
  let _ =
    OptParser.add opt ~short_name:'o' ~long_name:"output"
      ~help:"Set output file name to <file>" output;
    OptParser.add opt ~short_name:'c' ~long_name:"compile"
      ~help:"Compile only" compile_only in
  let inputs =
    OptParser.parse_argv opt in
  let o =
    get_option output in
  let table =
    List.fold_left
      InterCode.add_file
      InterCode.empty @@
      List.filter (flip Filename.check_suffix ".ho") @@
      Array.to_list @@ Sys.readdir "." in
    if inputs = [] then
      OptParser.usage opt ()
    else if get_option compile_only then
      List.iter (fun input ->
		   compile table (if o = "" then (file input ".ho") else o) input)
	inputs
    else
      build table inputs (if o = "" then "a.abc" else o)

let _ =
  if not !Sys.interactive then
    main ()


