open OptParse

let (@@) f g =  f g
let ($) f g x = f (g x)
let flip f x y = f y x
let tee f x = try ignore (f x); x with _ -> x

(* options *)
let opt_parser =
  OptParser.make ~version:"<VERSION>" ~usage:"habc [options] <file>" ()

let default_template =
  let base =
    Filename.dirname Sys.executable_name in
    if Sys.os_type = "Win32" then
      Filename.concat base "./template.xml"
    else
      Filename.concat base "../lib/template.xml"

let template =
  StdOpt.str_option 
    ~default:default_template 
    ~metavar:"TEMPLATE" ()

let output =
  StdOpt.str_option 
    ~default:"a.swf"
    ~metavar:"OUTPUT" ()

let verbose =
  StdOpt.store_true ()

let _ =
  OptParser.add opt_parser
    ~short_name:'t' 
    ~long_name:"template" 
    ~help:"set swf template name" template;
  OptParser.add opt_parser
    ~short_name:'o' 
    ~long_name:"output"
    ~help:"set swf output name" output;
  OptParser.add opt_parser
    ~short_name:'V'
    ~long_name:"verbose"
    ~help:"set verbose mode" verbose

(* shell *)
let debug str =
  if Opt.get verbose then
    Printf.eprintf "--> %s\n" str

let system cmd =
  debug cmd;
  ignore @@ Unix.system cmd

let rm path =
  debug @@ Printf.sprintf "rm %s" path;
  Unix.unlink path

let chop name =
  try
    Filename.chop_extension name
  with _ ->
    name

let make_temp ext =
  Printf.sprintf "%s.%s" (chop @@ Opt.get output) ext

let abc_of_scm scm =
  let abc = 
    make_temp "abc" in
    system @@ Printf.sprintf "habc-scm -o%s %s" abc scm;
    abc

let axml_of_abc abc =
  let axml =
    make_temp "axml" in
    system @@ Printf.sprintf "habc-xml %s > %s" abc axml;
    rm abc;
    axml

let xml_of_axml axml =
  let buffer =
    Buffer.create 0 in
  let abc =
    Std.input_file axml in
  let _ =
    Buffer.add_substitute 
      buffer
      (function "ABC" -> abc | "Label" -> "Main" | s -> s) @@ 
      Std.input_file @@ Opt.get template in
  let xml =
    make_temp "xml" in
  let ch =
    open_out xml in
    Buffer.output_buffer ch buffer;
    close_out ch;
    rm axml;
    xml
  
let swf_of_xml xml =
  let swf =
    make_temp "swf" in
    system @@ Printf.sprintf "swfmill xml2swf %s %s" xml swf;
    rm xml

let _ =
  let inputs =
    OptParser.parse_argv opt_parser in
    if inputs = [] then
      OptParser.usage opt_parser ()
    else 
      List.iter (ignore $ swf_of_xml $xml_of_axml $ axml_of_abc $ abc_of_scm) inputs
