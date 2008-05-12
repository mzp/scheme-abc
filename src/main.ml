open Base
open OptParse

let generate path stream =
  let ast =
    Lisp.compile stream in
  let abc = 
    Ast.generate ast in
  let bytes =
    Abc.bytes_of_abc abc in
  let ch = 
    open_out_bin path in
    Bytes.output_bytes ch bytes;
    close_out ch

let get_option x =
   Option.get @@ x.Opt.option_get ()

let main () =
  let opt =
    OptParser.make () in
  let output =
    StdOpt.str_option ~default:"a.abc" ~metavar:"OUTPUT" () in
  let _ =
    OptParser.add opt ~short_name:'o' ~long_name:"output" ~help:"output name" output in
  let inputs =
    OptParser.parse_argv opt in
    if inputs = [] then
     OptParser.usage opt ()
    else begin
     let ch =
       open_in (List.hd inputs) in
       generate (get_option output) @@ Stream.of_channel ch;
       close_in ch
    end

let _ =
  if not !Sys.interactive then begin
    main ()
  end
  
