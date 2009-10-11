open Base
open OptParse

type t = <color:int*int*int; size:int*int; main_class:string; output:string>

let opt_parser =
  OptParser.make
    ~version:Config.version
    ~usage:"habc-link [options] <file>" ()

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

let width =
  int_option
    ~default:(20 * 800)
    ~metavar:"<width>"
    ~short_name:'W'
    ~long_name:"width"
    ~help:"Set stage width by twips" ()

let height =
  int_option
    ~default:600
    ~metavar:"<height>"
    ~short_name:'H'
    ~long_name:"height"
    ~help:"Set stage height by pixel" ()

let main_class =
  str_option
    ~default:"Main"
    ~metavar:"<main_class>"
    ~short_name:'m'
    ~long_name:"main"
    ~help:"Set main class" ()

let output =
  str_option
    ~default:"a.swf"
    ~metavar:"<output swf>"
    ~short_name:'o'
    ~long_name:"output"
    ~help:"Set output name" ()

let red =
  int_option
    ~default:134
    ~metavar:"<red>"
    ~long_name:"red"
    ~help:"stage background color(red)" ()

let green =
  int_option
    ~default:156
    ~metavar:"<green>"
    ~long_name:"green"
    ~help:"stage background color(green)" ()

let blue =
  int_option
    ~default:167
    ~metavar:"<blue>"
    ~long_name:"blue"
    ~help:"stage background color(blue)" ()

let parse argv =
  let inputs =
    OptParser.parse opt_parser argv in
    inputs,{|
      color      = (Opt.get red, Opt.get green, Opt.get blue);
      main_class = Opt.get main_class;
      size       = (Opt.get width, Opt.get height);
      output     = Opt.get output
    |}

let parse_argv () =
  parse Sys.argv
