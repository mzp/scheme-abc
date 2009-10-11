open Base
open OUnit
open CmdOpt
open ExtString

let ok x f y =
  assert_equal x @@ f @@ snd @@ parse @@ Array.of_list @@ String.nsplit y " "

let _ = begin "cmdOpt.ml" >::: [
  "filename" >:: begin fun () ->
    assert_equal ["foo"]        @@ fst @@ parse [| "foo" |];
    assert_equal ["foo"; "bar"] @@ fst @@ parse [| "foo"; "bar" |]
  end;
  "size" >:: begin fun () ->
    ok (100,200) (fun {size} -> size) "-W 100 -H 200";
    ok (100,200) (fun {size} -> size) "--width=100 --height=200";
  end;
  "color" >:: begin fun () ->
    ok (1,2,3) (fun {color}-> color) "--red=1 --green=2 --blue=3"
  end;
  "main class" >:: begin fun () ->
    ok "foo" (fun {main_class}->main_class) "-m foo";
    ok "foo" (fun {main_class}->main_class) "--main=foo"
  end
] end +> run_test_tt_main
