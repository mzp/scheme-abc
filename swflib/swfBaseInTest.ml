open Base
open OUnit
open SwfBaseIn

let ok x f y =
  let s =
    Stream.of_list y in
    assert_equal x (f s);
    assert_equal ~msg:"empty" None (Stream.peek s)

let _ = begin "swfBaseIn.ml" >::: [
  "ui8" >:: begin fun () ->
    ok 0x12 ui8 [0x12];
    ok 0xFF ui8 [0xFF];
  end;
  "ui16" >:: begin fun () ->
    ok 0x1234 ui16 [0x34; 0x12]
  end;
  "ui24" >:: begin fun () ->
    ok 0x123456 ui24 [0x56; 0x34; 0x12]
  end;
  "ui32" >:: begin fun () ->
    ok 0x12345678l ui32 [0x78; 0x56; 0x34; 0x12]
  end;
  "si8" >:: begin fun () ->
    ok ~-1  si8 [0xFF];
    ok 0x12 si8 [0x12]
  end;
  "si16" >:: begin fun () ->
    ok ~-1    si16 [0xFF; 0xFF];
    ok 0x1234 si16 [0x34; 0x12]
  end;
  "si24" >:: begin fun () ->
    ok ~-1      si24 [0xFF; 0xFF; 0xFF];
    ok 0x123456 si24 [0x56; 0x34; 0x12]
  end;
  "si32" >:: begin fun () ->
    ok (-1l)       si32 [0xFF; 0xFF; 0xFF; 0xFF];
    ok 0x12345678l si32 [0x78; 0x56; 0x34; 0x12]
  end;
] end +> run_test_tt_main
