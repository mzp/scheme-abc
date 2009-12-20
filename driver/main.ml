open Base
open Rule
open CmdOpt

let m4_opt xs =
  String.concat " " @@
    List.map (fun (x,y) ->
		Printf.sprintf "-D%s=%s" x y)
    xs


let rules = [
  one_to_one "scm" "ho" begin fun {scm = {scm_cmd=scm_cmd; includes=includes} } input output ->
       [Printf.sprintf "%s -c -I %s -o %s %s"
	  scm_cmd includes output input ]
  end;
  many_to_one ["scm"] "abc" begin fun {scm = {scm_cmd=scm_cmd; includes=includes;link_std=link_std}} inputs output ->
    [Printf.sprintf "%s -I %s -o %s %s %s"
       scm_cmd includes output
       (if link_std then "std.ho" else "")
     @@ String.concat " " inputs ]
  end;
  many_to_one ["scm";"ho"] "abc" begin fun {scm = {scm_cmd=scm_cmd; includes=includes}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s"
	  scm_cmd includes output @@ String.concat " " inputs ]
  end;
  many_to_one ["ho"] "abc" begin fun {scm = {scm_cmd=scm_cmd; includes=includes}} inputs output ->
       [Printf.sprintf "%s -I %s -o %s %s"
	  scm_cmd includes output @@ String.concat " " inputs ]
  end;
  (* link *)
  many_to_one ["abc"] "swf" begin fun { link = {
					  link_cmd=link_cmd;
					  size=(w,h);
					  bg_color = {Color.red=r; green=g; blue=b};
					  libs=libs;
					  use_network=use_network
					}}
    inputs output ->
      [Printf.sprintf "%s --width=%d --height=%d --red=%d --green=%d --blue=%d --main=boot.Boot --output=%s %s %s %s"
	 link_cmd w h r g b output
	 (if use_network then "--use-network" else "")
	 (String.concat " " libs) @@ String.concat " " inputs]
  end;
]

let debug verbose str =
  if verbose then begin
    Printf.eprintf "--> %s\n" str;
    flush stderr;
    flush stdout
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
		 | Unix.WEXITED n | Unix.WSIGNALED n | Unix.WSTOPPED n ->
		     prerr_endline "BUILD ERROR";
		     exit n)
    commands

let verbose {general={verbose=verbose}} s =
    if verbose then
      prerr_endline s

let main _ =
  let {inputs=inputs; output=output} as ctx =
    CmdOpt.parse Sys.argv in
  let _ =
    verbose ctx @@ Printf.sprintf "Target: [%s] => %s\n" (String.concat "; " inputs) output in
  let commands =
    Rule.commands ctx rules inputs output in
    if commands = [] then
      failwith "no rule"
    else if ctx.general.just_print then
      List.iter print_endline commands
    else begin
      execute ctx commands;
      if not ctx.general.keep_files then
	List.iter Sys.remove @@ Rule.temp_files ctx rules inputs output
    end

let _ =
  if not !Sys.interactive then
    main ()
