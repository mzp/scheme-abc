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
  template: string;
  size: int * int;
  bg_color: string
}

type swfx = {
  swfx_cmd:string;
}

type general = {
  verbose: bool;
}

type t = {
  inputs:  string list;
  output: string;
  output_type: output_type;
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
      ~default:"habc-scm"
      ~metavar:"<cmd>"
      ~long_name:"scm"
      ~help:"Use <cmd> to compile scm to abc" () in
  let includes =
    str_option
      ~default:""
      ~metavar:"<dir ..>"
      ~short_name:'I'
      ~help:"Add <dir ..> to the list of include directories" () in
    fun () -> {
       scm_cmd  = Opt.get cmd;
       includes = Opt.get includes;
     }

let abc =
  let cmd =
    str_option
      ~default:"habc-abc"
      ~metavar:"<cmd>"
      ~long_name:"abcx"
      ~help:"Use <cmd> to compile abc to abcx" () in
    fun () -> {
       abc_cmd = Opt.get cmd
     }

let abcx =
  let width =
    int_option
      ~default:300
      ~metavar:"<width>"
      ~long_name:"width"
      ~help:"stage width" () in
  let height =
    int_option
      ~default:300
      ~metavar:"<height>"
      ~long_name:"height"
      ~help:"stage height" () in
  let bg_color =
    str_option
      ~default:"0xFFFFFF"
      ~metavar:"<color>"
      ~long_name:"bg"
      ~help:"stage background color" () in
  let template =
    str_option
      ~default:Config.OS.default_template
      ~metavar:"<tempalte>"
      ~long_name:"template"
      ~help:"swfx template" () in
    fun () -> {
       bg_color =  Opt.get bg_color;
       size     = (Opt.get width,Opt.get height);
       template =  Opt.get template
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
      ~short_name:'V'
      ~long_name:"verbose"
      ~help:"Print calls to external command" () in
    fun () -> {
      verbose = Opt.get verbose;
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
      ~default:""
      ~metavar:"<file>"
      ~short_name:'o'
      ~help:"Set output filename" () in
  let inputs =
     OptParser.parse opt_parser args in
    match inputs with
	[] ->
	  OptParser.usage opt_parser ();
	  exit 0
      | x::_ ->
	  let o =
	    if Opt.get output <> "" then
	      Opt.get output
	    else
	      match output_type () with
		  Ho   ->  "a.ho"
		| Abc  ->  "a.abc"
		| Abcx -> "a.abcx"
		| Swfx -> "a.swfx"
		| Swf  -> "a.swf" in
	    {
	      inputs      = inputs;
	      output      = o;
	      output_type = output_type ();
	      general     = general ();
	      scm         = scm ();
	      abc         = abc ();
	      abcx        = abcx ();
	      swfx        = swfx ();
	    }

