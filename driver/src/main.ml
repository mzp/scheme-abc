open Base
open Rule
open CmdOpt

let m4_opt xs =
  String.concat " " @@
    List.map (fun (x,y) ->
		Printf.sprintf "-D%s=%s" x y)
    xs


let rules = [
  one_to_one "scm" "ho"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes; link_std=link_std}} input output ->
       [Printf.sprintf "%s -c -I %s -o %s %s %s"
	  scm_cmd includes output
	  (if link_std then "std.ho" else "")
	  input ]);
  many_to_one ["scm"] "abc"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes;link_std=link_std}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s %s"
	  scm_cmd includes output
	  (if link_std then "std.ho" else "")
	@@ String.concat " " inputs ]);
  many_to_one ["scm";"ho"] "abc"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s"
	  scm_cmd includes output @@ String.concat " " inputs ]);
  many_to_one ["ho"] "abc"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s"
	  scm_cmd includes output @@ String.concat " " inputs ]);
  one_to_one "abc" "abcx"
    (fun {abc = {abc_cmd=abc_cmd}} input output ->
       [Printf.sprintf "%s %s > %s" abc_cmd input output]);
  one_to_one "abcx" "swfx"
    (fun { abcx = {
	     abcx_cmd = m4;
	     template = template;
	     size     = (w,h);
	     bg_color = {Color.red=r; green=g; blue=b};
	   }} input output ->
       [Printf.sprintf "%s -I. %s %s > %s"
	  m4
	  (m4_opt [
	     "__ABCX__"      ,input;
	     "__MAIN_CLASS__","boot.Boot";
	     "__WIDTH__"     ,string_of_int w;
	     "__HEIGHT__"    ,string_of_int h;
	     "__BG_RED__"    ,string_of_int r;
	     "__BG_GREEN__"  ,string_of_int g;
	     "__BG_BLUE__"   ,string_of_int b;
	   ])
	  template output]);
  one_to_one "swfx" "swf"
    (fun { swfx = { swfx_cmd=swfx_cmd } } input output ->
       [Printf.sprintf "%s xml2swf %s %s" swfx_cmd input output]);
]

let debug verbose str =
  if verbose then begin
    Printf.eprintf "--> %s\n" str;
    flush stderr
  end

let system {general={verbose=verbose}} cmd =
  let cmd' =
    Str.global_replace (Str.regexp "\\\\") "/" cmd in
    debug verbose cmd';
    Unix.system cmd'

let execute ctx commands =
  List.iter (fun s ->
	       match system ctx s with
		   Unix.WEXITED 0 ->
		     ()
		 | _ ->
		     prerr_endline "BUILD ERROR";
		     exit 1)
    commands

let verbose {general={verbose=verbose}} s =
    if verbose then
      prerr_endline s

let main _ =
  let {inputs=inputs; output=output} as ctx =
    CmdOpt.parse () in
  let _ =
    verbose ctx @@ Printf.sprintf "Target: [%s] => %s\n" (String.concat "; " inputs) output in
  let commands =
    Rule.commands ctx rules inputs output in
    if ctx.general.just_print then
      List.iter print_endline commands
    else begin
      execute ctx commands;
      if not ctx.general.keep_files then
	List.iter Sys.remove @@ Rule.temp_files ctx rules inputs output
    end

let _ =
  if not !Sys.interactive then
    main ()
