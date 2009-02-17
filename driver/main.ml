open Base
open Rule
open CmdOpt

let rules = [
  one_to_one "scm" "ho"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes}} input output ->
       [Printf.sprintf "%s -c -I %s -o %s %s"
	  scm_cmd includes output input ]);
  many_to_one ["scm"] "abc"
    (fun {scm = {scm_cmd=scm_cmd; includes=includes}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s"
	  scm_cmd includes output @@ String.concat " " inputs ]);
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
	     template=template;
	     size=(w,h);
	     bg_color=bg_color;
	   }} _ _ ->
       ["echo not yet"]);
  one_to_one "swfx" "swf"
    (fun { swfx = { swfx_cmd=swfx_cmd } } input output ->
       [Printf.sprintf "%s xml2swf %s %s" swfx_cmd input output]);
]

let debug verbose str =
  if verbose then
    Printf.eprintf "--> %s\n" str

let system {general={verbose=verbose}} cmd =
  debug verbose cmd;
  Unix.system cmd

(*
let xml_of_axml input output =
  let buffer =
    Buffer.create 0 in
  let abc =
    Std.input_file input in
  let _ =
    Buffer.add_substitute
      buffer
      (function "ABC" -> abc | "Label" -> "Main" | s -> s) @@
      Std.input_file @@ Opt.get template in
  let ch =
    open_out output in
    Buffer.output_buffer ch buffer;
    close_out ch;
    output
*)

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
    else
      execute ctx commands

let _ =
  if not !Sys.interactive then
    main ()
