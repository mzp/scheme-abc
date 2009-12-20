open Base
open OUnit
open CmdOpt
open ExtString

let ok x f y =
  let argv =
    Array.of_list @@ String.nsplit ("./foo " ^ y ^ " eof") " " in
    assert_equal ~printer:Std.dump x @@ f @@ parse argv

(*
type scm = {
  scm_cmd:  string;
  includes: string;
  link_std: bool
}
*)

let _ = begin "cmdOpt.ml" >::: [
  "filename" >:: begin fun () ->
    ok ["foo"; "eof"]        (fun t -> t.inputs) "foo";
    ok ["foo"; "bar"; "eof"] (fun t -> t.inputs) "foo bar"
  end;
  "output" >:: begin fun () ->
    ok "foo.swf" (fun t -> t.output) "-ofoo";
  end;
  "verbose" >:: begin fun () ->
    ok false (fun t -> t.general.verbose) "";
    ok true (fun t -> t.general.verbose) "--verbose";
  end;
  "just-print" >:: begin fun () ->
    ok false (fun t -> t.general.just_print) "";
    ok true (fun t -> t.general.just_print) "--just-print";
  end;
  "keep files" >:: begin fun () ->
    ok false (fun t -> t.general.keep_files) "";
    ok true (fun t -> t.general.keep_files) "--keep-files"
  end;
  "link cmd" >:: begin fun () ->
    ok (Config.bin_dir ^ "/habc-link") (fun t -> t.link.link_cmd) "";
    ok "foo" (fun t -> t.link.link_cmd) "--link=foo"
  end;
  "size" >:: begin fun () ->
    ok (800*20, 600*20) (fun t -> t.link.size) "";
    ok (20,40) (fun t -> t.link.size) "--width=1 --height=2";
    ok (20,40) (fun t -> t.link.size) "-W1 -H2";
  end;
  "color" >:: begin fun () ->
    ok (Color.rgb 134 156 167) (fun t -> t.link.bg_color) "";
    ok (Color.rgb 0 0 0) (fun t -> t.link.bg_color) "--bg=black";
    ok (Color.rgb 1 2 3) (fun t -> t.link.bg_color) "--bg=rgb(1,2,3)";
    ok (Color.rgb 1 2 3) (fun t -> t.link.bg_color) "\"--bg=rgb(1, 2, 3)\"";
  end;
  "lib" >:: begin fun () ->
    ok [] (fun t -> t.link.libs) "";
    ok ["foo.swf"; "bar.swf"] (fun t -> t.link.libs) "-Lfoo.swf:bar.swf";
    ok ["foo.swf"; "bar.swf"] (fun t -> t.link.libs) "--libs foo.swf:bar.swf";
  end;
  "scm" >:: begin fun () ->
    ok (Config.bin_dir ^ "/habc-scm") (fun t -> t.scm.scm_cmd) "";
    ok "foo" (fun t -> t.scm.scm_cmd) "--scm=foo"
  end;
  "includes" >:: begin fun () ->
    ok (Config.lib_dir ^":.:") (fun t -> t.scm.includes) "";
    ok (Config.lib_dir ^":.:foo") (fun t -> t.scm.includes) "-Ifoo"
  end;
  "link std" >:: begin fun () ->
    ok true (fun t -> t.scm.link_std) "";
    ok false (fun t -> t.scm.link_std) "--no_std";
  end;
  "use network" >:: begin fun () ->
    ok false (fun t -> t.link.use_network) "";
    ok true (fun t -> t.link.use_network) "--use-network";
  end
] end +> run_test_tt_main
