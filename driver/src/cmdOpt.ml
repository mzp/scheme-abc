open Base
open OptParse

type output_type =
    Ho | Abc | Abcx | Swfx | Swf

type scm = {
  scm_cmd:    string;
  includes: string
}

type abc = {
  abc_cmd: string
}

type abcx = {
  abcx_cmd: string;
  template: string;
  size: int * int;
  bg_color: string;
  main_class: string;
}

type swfx = {
  swfx_cmd:string;
}

type general = {
  verbose:    bool;
  just_print: bool;
  keep_files: bool;
}

type t = {
  inputs:  string list;
  output: string;
  general: general;
  scm:  scm;
  abc:  abc;
  abcx: abcx;
  swfx: swfx;
}

let opt_parser =
  OptParser.make ~version:Config.version ~usage:"habc [options] <file>" ()

let str_option ~default ~metavar ?short_name ?long_name ~help () =
  let store =
    StdOpt.str_option ~default ~metavar () in
  let _ =
    OptParser.add opt_parser
      ?short_name
      ?long_name ~help store in
    store

let int_option ~default ~metavar ?short_name ?long_name ~help () =
  let store =
    StdOpt.int_option ~default ~metavar () in
  let _ =
    OptParser.add opt_parser
      ?short_name ?long_name ~help store in
    store

let bool_option ~default ?short_name ?long_name ~help () =
  let store =
    if not default then
      StdOpt.store_true ()
    else
      StdOpt.store_false () in
  let _ =
    OptParser.add opt_parser
      ?short_name ?long_name ~help store in
    store

let scm =
  let cmd =
    str_option
      ~default:(Config.bin_dir ^ "/habc-scm" ^ Config.exe)
      ~metavar:"<cmd>"
      ~long_name:"scm"
      ~help:"Use <cmd> to compile scm to abc" () in
  let includes =
    str_option
      ~default:""
      ~metavar:"<dir ..>"
      ~short_name:'I'
      ~help:"Add <dir ..> to the list of include directories" () in
  let default =
    String.concat Config.path_sep Config.default_includes in
    fun () -> {
       scm_cmd  = Opt.get cmd;
       includes = default ^ Config.path_sep ^ Opt.get includes;
     }

let abc =
  let cmd =
    str_option
      ~default:(Config.bin_dir ^ "/habc-xml" ^ Config.exe)
      ~metavar:"<cmd>"
      ~long_name:"abcx"
      ~help:"Use <cmd> to compile abc to abcx" () in
    fun () -> {
       abc_cmd = Opt.get cmd
     }

let abcx =
  let cmd =
    str_option
      ~default:"m4"
      ~metavar:"<cmd>"
      ~long_name:"swfx"
      ~help:"Use <cmd> to compile abcx to swfx" () in
  let width =
    int_option
      ~default:2048
      ~metavar:"<width>"
      ~short_name:'W'
      ~long_name:"width"
      ~help:"stage width" () in
  let height =
    int_option
      ~default:800
      ~metavar:"<height>"
      ~short_name:'H'
      ~long_name:"height"
      ~help:"stage height" () in
  let bg_color =
    str_option
      ~default:"0xFFFFFF"
      ~metavar:"<color>"
      ~long_name:"bg"
      ~help:"stage background color" () in
  let main_class =
    str_option
      ~default:"Main"
      ~metavar:"<class>"
      ~long_name:"main_class"
      ~help:"Main Class of swf" () in
  let template =
    str_option
      ~default:Config.default_template
      ~metavar:"<tempalte>"
      ~long_name:"template"
      ~help:"swfx template" () in
    fun () -> {
      abcx_cmd = Opt.get cmd;
      bg_color = Opt.get bg_color;
      size     = (Opt.get width,Opt.get height);
      template = Opt.get template;
      main_class = Opt.get main_class
    }

let swfx =
  let cmd =
    str_option
      ~default:"swfmill"
      ~metavar:"<cmd>"
      ~long_name:"swf"
      ~help:"Use <cmd> to compile swfx to swf" () in
    fun () -> {
       swfx_cmd = Opt.get cmd
    }

let general =
  let verbose =
    bool_option
      ~default:false
      ~short_name:'v'
      ~long_name:"verbose"
      ~help:"Print calls to external command" () in
  let just_print =
    bool_option
      ~default:false
      ~short_name:'n'
      ~long_name:"just-print"
      ~help:"Don't actually run any commands; just print them" () in
  let keep_files =
    bool_option
      ~default:false
      ~short_name:'k'
      ~long_name:"keep-files"
      ~help:"Keep temporary files" () in
    fun () -> {
      verbose    = Opt.get verbose;
      just_print = Opt.get just_print;
      keep_files = Opt.get keep_files
    }

let output_type =
  let ho =
    bool_option
      ~default:false ~short_name:'c' ~help:"compile only" () in
  let abc =
    bool_option
      ~default:false ~long_name:"abc-stage" ~help:"(no doc)" () in
  let abcx =
    bool_option
      ~default:false ~long_name:"abcx-stage" ~help:"(no doc)" () in
  let swfx =
    bool_option
      ~default:false ~long_name:"swfx-stage" ~help:"(no doc)" () in
    fun () ->
      if Opt.get ho then
	Ho
      else if Opt.get abc then
	Abc
      else if Opt.get abcx then
	Abcx
      else if Opt.get swfx then
	Swfx
      else
	Swf

let parse args =
  let output =
    str_option
      ~default:"a"
      ~metavar:"<file>"
      ~short_name:'o'
      ~help:"Set output filename" () in
  let inputs =
     OptParser.parse_argv opt_parser in
    match inputs with
	[] ->
	  OptParser.usage opt_parser ();
	  exit 0
      | x::_ ->
	  let o =
	    Opt.get output ^
	      match output_type () with
		  Ho   -> ".ho"
		| Abc  -> ".abc"
		| Abcx -> ".abcx"
		| Swfx -> ".swfx"
		| Swf  -> ".swf" in
	    {
	      inputs      = inputs;
	      output      = o;
	      general     = general ();
	      scm         = scm  ();
	      abc         = abc  ();
	      abcx        = abcx ();
	      swfx        = swfx ();
	    }

