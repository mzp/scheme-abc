open Base
open OUnit
open OutMsg

let ok x y =
  assert_equal x @@ serialize y

let _ = begin "outMsg.ml" >::: [
  "zoom in" >:: begin fun () ->
    ok (0x00, "") ZoomIn
  end;
  "zoom out" >:: begin fun () ->
    ok (0x01, "") ZoomOut
  end;
  "quality" >:: begin fun () ->
    ok (0x04, "BEST\000") @@ Quality Best
  end
] end +> run_test_tt_main

