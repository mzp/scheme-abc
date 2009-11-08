open OUnit
open Base
open Rule


let rules = [
  one_to_one "c" "o"
    (fun ctx input output ->
       assert_equal 42 ctx;
       [Printf.sprintf "gcc -c -o%s %s" output input]);
  one_to_one "c" "s"
    (fun ctx input output ->
       assert_equal 42 ctx;
       (* this option is not correct *)
       [Printf.sprintf "gcc -s -o%s %s" output input]);
  many_to_one ["o"] "exe"
    (fun ctx inputs output ->
       assert_equal 42 ctx;
       [Printf.sprintf "gcc -o%s %s" output @@ String.concat " " inputs]);
  many_to_one ["o"] "o"
    (fun ctx inputs output ->
       assert_equal 42 ctx;
       [Printf.sprintf "gcc -o%s %s" output @@ String.concat " " inputs]);
  many_to_one ["o"; "c"] "o"
    (fun ctx inputs output ->
       assert_equal 42 ctx;
       [Printf.sprintf "gcc -o%s %s" output @@ String.concat " " inputs])
]

let cmds =
  commands 42 rules

let temp =
  temp_files 42 rules

let tests =
  ("rule.ml" >::: [
     "If nodes is direct connected, return it" >::
       (fun _ ->
	  assert_equal ["gcc -s -ofoo.s foo.c"] @@
	    cmds ["foo.c"] "foo.s");
     "If two routes exits, choice shorter one." >::
       (fun _ ->
	  assert_equal ["gcc -c -ofoo.o foo.c"] @@
	    cmds ["foo.c"] "foo.o");
     "If nodes is not direct connected, return the routes" >::
       (fun _ ->
	  assert_equal
	    ["gcc -c -ofoo.o foo.c";
	     "gcc -ofoo.exe foo.o"] @@
	    cmds ["foo.c"] "foo.exe");

     "If commands generate tmp files, return it" >::
       (fun _ ->
	  assert_equal [] @@ temp ["foo.c"] "foo.o";
	  assert_equal ["foo.o"] @@ temp ["foo.c"] "foo.exe");
     "contain loop" >::
       (fun _ ->
	  assert_equal ~printer:Std.dump ["gcc -ofoo.o foo.o bar.o"] @@ cmds ["foo.o"; "bar.o"] "foo.o";
	  assert_equal ~printer:Std.dump ["gcc -ofoo.o foo.o bar.c"] @@ cmds ["foo.o"; "bar.c"] "foo.o")
]) +> run_test_tt_main
