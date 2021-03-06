open Base
open OptParse

type output_type =
    Ho | Abc | Swf

type scm = {
  scm_cmd:  string;
  includes: string;
  link_std: bool
}

type link = {
  link_cmd: string;
  size: int * int;
  bg_color: Color.t;
  libs: string list;
  use_network: bool;
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
  link: link;
}

module M = OptParseUtil.Make(
  struct
    let v =
      OptParser.make ~version:Config.version ~usage:"habc [options] <file>" ()
  end)
open M

let _ =
  str_callback ~long_name:"conf" ~help:"Print configure and exit"
    (fun _ ->
       Printf.printf "version:          %s\n" @@ Std.dump Config.version;
       Printf.printf "bin_dir:          %s\n" @@ Std.dump Config.bin_dir;
       Printf.printf "share_dir:        %s\n" @@ Std.dump Config.share_dir;
       Printf.printf "lib_dir:          %s\n" @@ Std.dump Config.lib_dir;
       Printf.printf "default_includes: %s\n" @@ Std.dump Config.default_includes;
       Printf.printf "default_template: %s\n" @@ Std.dump Config.default_template;
       Printf.printf "path_sep:         %s\n" @@ Std.dump Config.path_sep;
       Printf.printf "exe:              %s\n" @@ Std.dump Config.exe;
       exit 0)

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
  let no_std =
    bool_option
      ~default:true
      ~long_name:"no_std"
      ~help:"without std library" () in
  let default =
    Config.default_includes
    +> List.filter Sys.file_exists
    +> String.concat Config.path_sep in
    fun () -> {
      scm_cmd  = Opt.get cmd;
      includes = default ^ Config.path_sep ^ Opt.get includes;
      link_std = Opt.get no_std
    }

let link =
  let cmd =
    str_option
      ~default:(Config.bin_dir ^ "/habc-link" ^ Config.exe)
      ~metavar:"<cmd>"
      ~long_name:"link"
      ~help:"Use <cmd> to compile abc to swf" () in
  let width =
    int_option
      ~default:800
      ~metavar:"<width>"
      ~short_name:'W'
      ~long_name:"width"
      ~help:"Set stage width by pixel" () in
  let height =
    int_option
      ~default:600
      ~metavar:"<height>"
      ~short_name:'H'
      ~long_name:"height"
      ~help:"Set stage height by pixel" () in
  let bg_color =
    str_option
      ~default:"rgb(134,156,167)"
      ~metavar:"<color>"
      ~long_name:"bg"
      ~help:"stage background color" () in
  let libs =
    str_option
      ~default:""
      ~metavar:"<libs>"
      ~short_name:'L'
      ~long_name:"libs"
      ~help:"linked library" () in
  let use_network =
    bool_option
      ~default:false
      ~long_name:"use-network"
      ~help:"enable use_network" () in
    fun () -> {
      link_cmd = Opt.get cmd;
      bg_color = Color.parse @@ Opt.get bg_color;
      size     = (20 * Opt.get width,20 * Opt.get height); (* convert pixel to twips *)
      libs     = Str.split (Str.regexp Config.path_sep) @@ Opt.get libs;
      use_network = Opt.get use_network
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
    fun () ->
      if Opt.get ho then
	Ho
      else if Opt.get abc then
	Abc
      else
	Swf

let output =
  str_option
    ~default:"a"
    ~metavar:"<file>"
    ~short_name:'o'
    ~help:"Set output filename" ()

let parse argv =
  let inputs =
    OptParser.parse ~first:1 opt_parser argv in
    if inputs = [] then begin
      OptParser.usage opt_parser ();
      exit 0
    end else
      let o =
	Opt.get output ^
	  match output_type () with
	      Ho   -> ".ho"
	    | Abc  -> ".abc"
	    | Swf  -> ".swf" in
	{
	  inputs  = inputs;
	  output  = o;
	  general = general ();
	  scm     = scm  ();
	  link    = link ();
	}
