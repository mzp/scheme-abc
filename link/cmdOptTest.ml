open Base
open OUnit
open CmdOpt
open ExtString

let ok x f y =
  let argv =
    Array.of_list @@ String.nsplit ("./foo " ^ y) " " in
    assert_equal ~printer:Std.dump x @@ f @@ snd @@ parse argv

let _ = begin "cmdOpt.ml" >::: [
  "filename" >:: begin fun () ->
    assert_equal ["foo"]        @@ fst @@ parse [| "./foo"; "foo" |];
    assert_equal ["foo"; "bar"] @@ fst @@ parse [| "./foo"; "foo"; "bar" |]
  end;
  "size" >:: begin fun () ->
    ok (100,200) (fun t -> t#size) "-W 100 -H 200";
    ok (100,200) (fun t -> t#size) "--width=100 --height=200";
  end;
  "color" >:: begin fun () ->
    ok (1,2,3) (fun t -> t#color) "--red=1 --green=2 --blue=3"
  end;
  "main class" >:: begin fun () ->
    ok "boot.Boot" (fun t -> t#main_class) "";
    ok "foo"  (fun t -> t#main_class) "-m foo";
    ok "foo"  (fun t -> t#main_class) "--main=foo"
  end;
  "out" >:: begin fun () ->
    ok "a.swf"   (fun t -> t#output) "";
    ok "foo.swf" (fun t -> t#output) "-o foo.swf";
    ok "foo.swf" (fun t -> t#output) "--output=foo.swf"
  end;
] end +> run_test_tt_main
