open Base
open OUnit
open BitsOut

let ok x y =
  assert_equal ~printer:Std.dump x @@ to_list y

let _ = begin "bitsOut.ml" >::: [
  "empty" >:: begin fun () ->
    ok [] @@ empty
  end;
  "put" >:: begin fun () ->
    let t1 =
      put empty ~width:1 ~bits:1 in
    let t2 =
      put t1 ~width:3 ~bits:1 in
      ok [0b1001_0000] t2
  end;
  "put & put" >:: begin fun () ->
    let t1 =
      put empty ~width:5 ~bits:1 in
    let t2 =
      put t1 ~width:5 ~bits:1 in
      ok [0b0000_1000] t1;
      ok [0b0000_1000; 0b0100_0000] t2;
  end;
  "padding" >:: begin fun () ->
    ok [0b1000_0000] @@ put empty ~width:1 ~bits:1
  end;
  "put 2 bytes" >:: begin fun () ->
    ok [0xFF; 0b1000_0000] @@ put empty ~width:9 ~bits:0x1FF;
  end;
  "invalid arguments" >:: begin fun () ->
    try
      ignore @@ put empty ~width:2 ~bits:0b100;
      assert_failure "not raise"
    with Invalid_argument _ ->
      ()
  end;
] end +> run_test_tt_main
