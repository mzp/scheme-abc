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

let error kind msg {filename=filename; lineno=lineno} =
  let ch =
    open_in filename in
    Printf.eprintf "%s:%d: %s,%s\n" filename lineno kind msg;
    prerr_endline @@ extract_line lineno ch;
    close_in ch

let generate path stream =
  try
    let ast =
      Lisp.compile stream in
    let abc = 
      Codegen.generate @@ ClosureTrans.trans @@ ClosTrans.trans ast in
    let bytes =
      Abc.to_bytes abc in
    let ch = 
      open_out_bin path in
      Bytes.output_bytes ch bytes;
      close_out ch
  with
      Lisp.Syntax_error (msg,loc) ->
	error "synatx error" msg loc

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

  
