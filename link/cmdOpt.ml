open Base
open OptParse

class type t = object
  method color:int*int*int
  method size:int*int
  method main_class:string
  method output:string
  method use_network:bool
end

module M = OptParseUtil.Make(
  struct
    let v =
      OptParser.make
	~version:Config.version
	~usage:"habc-link [options] <file>" ()
  end)
open M

let width =
  int_option
    ~default:(20 * 800)
    ~metavar:"<width>"
    ~short_name:'W'
    ~long_name:"width"
    ~help:"Set stage width by twips" ()

let height =
  int_option
    ~default:(20 * 600)
    ~metavar:"<height>"
    ~short_name:'H'
    ~long_name:"height"
    ~help:"Set stage height by pixel" ()

let main_class =
  str_option
    ~default:"boot.Boot"
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

let use_network =
  bool_option
    ~default:false
    ~long_name:"use-network"
    ~help:"Set use_network option" ()

let parse argv =
  let inputs =
    OptParser.parse ~first:1 opt_parser argv in
    inputs,{|
      color       = (Opt.get red, Opt.get green, Opt.get blue);
      main_class  = Opt.get main_class;
      size        = (Opt.get width, Opt.get height);
      output      = Opt.get output;
      use_network = Opt.get use_network
    |}

let parse_argv () =
  parse Sys.argv
