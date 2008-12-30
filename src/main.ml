open Base
open OptParse
open Node

let rec extract_line n ch =
  if n = 0 then
    input_line ch
  else begin
    ignore @@ input_line ch;
    extract_line (n-1) ch
  end

let error kind {value=msg; filename=filename; lineno=lineno; start_pos=a; end_pos=b} =
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

let generate path stream =
  try
    let ast =
      ClosTrans.trans @@ Lisp.compile stream in
    let ast' =
      ClosureTrans.trans @@
	ModuleTrans.trans @@ BindCheck.check  ast in
    let abc =
      curry Codegen.generate @@ VarResolve.trans ast'  in
    let bytes =
      Abc.to_bytes abc in
    let ch =
      open_out_bin path in
      Bytes.output_bytes ch bytes;
      close_out ch
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
    | BindCheck.Unbound_class ({Node.value=(ns,name)} as loc) ->
	let name =
	  ns ^ "." ^ name in
	  error ("unbound class") {loc with Node.value = name};
	  exit 1
    | BindCheck.Unbound_method loc ->
	error ("unbound method") loc;
	exit 1


let get_option x =
   Option.get @@ x.Opt.option_get ()

let main () =
  let opt =
    OptParser.make ~version:"0.1.0" () in
  let output =
    StdOpt.str_option ~default:"a.abc" ~metavar:"<output>" () in
  let _ =
    OptParser.add opt ~short_name:'o' ~long_name:"output"
      ~help:"Set output file name to <file>" @@ output in
  let inputs =
    OptParser.parse_argv opt in
    if inputs = [] then
      OptParser.usage opt ()
    else
      generate (get_option output) @@ Node.of_file @@ List.hd inputs


let _ =
  if not !Sys.interactive then
    main ()


