open Base
open OUnit
open SwfBaseOut

let ok_i x y =
  assert_equal x @@ to_int_list [ y ]

let ok_b x y =
  assert_equal (to_int_list x) (to_int_list y)

let _ = begin "swfBaseOut.ml" >::: [
  "equality" >:: begin fun () ->
    assert_equal (Si8 0) (Si8 0)
  end;
  "Si8" >:: begin fun () ->
    ok_i  [0x7F] @@ Si8 0x7F;
    ok_i  [0xFF] @@ Si8 (~-1);
  end;
  "Si16" >:: begin fun () ->
    ok_i [0x7F; 0x00] @@ Si16 0x7F;
    ok_i [0xFF; 0x7F] @@ Si16 0x7FFF;
    ok_i [0xFF; 0xFF] @@ Si16 (~-1);
  end;
  "Si24" >:: begin fun () ->
    ok_i [0x7F; 0; 0] @@ Si24 0x7F;
    ok_i [0x56; 0x34; 0x12] @@ Si24 0x123456;
    ok_i [0xFF; 0xFF; 0xFF] @@ Si24 (~-1);
  end;
  "Si32" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0x7F] @@ Si32 0x7FFFFFFFl;
    ok_i [0xFF; 0xFF; 0xFF; 0xFF] @@ Si32 (-1l);
  end;
  "Ui8" >:: begin fun () ->
    ok_i [0x0] @@ Ui8 0x0;
    ok_i [0xFF] @@ Ui8 0xFF;
  end;
  "Ui16" >:: begin fun () ->
    ok_i [0x00; 0x00] @@ Ui16 0;
    ok_i [0xFF; 0xFF] @@ Ui16 0xFFFF;
    ok_i [0x34; 0x12] @@ Ui16 0x1234;
  end;
  "Ui24" >:: begin fun () ->
    ok_i [0;0;0] @@ Ui24 0;
    ok_i [0x56; 0x34; 0x12] @@ Ui24 0x123456;
    ok_i [0xFF; 0xFF; 0xFF] @@ Ui24 0xFFFFFF;
  end;
  "Ui32" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0x7F] @@ Ui32 0x7FFFFFFFl;
    ok_i [0xFF; 0xFF; 0xFF; 0xFF] @@ Ui32 0xFFFFFFFFl;
  end;
  "Ui64" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0xFF; 0;0;0;0] @@
      Ui64 0xFFFFFFFFL;
  end;
  "Fixed" >:: begin fun () ->
    ok_b [Ui16 8; Ui16 7] [Fixed 7.5];
    ok_b [Ui16 0; Ui16 0xFFFF] [Fixed 65535.0];
    ok_b [Ui16 0xFFFF; Ui16 0xFFFF] [Fixed 65535.65535]
  end;
  "Fixed8" >:: begin fun () ->
    ok_b [Ui16 0xFF; Ui16 0xFF] [Fixed 256.256];
    ok_b [Ui8 42; Ui8 1] [Fixed 42.1]
  end
]end +> run_test_tt_main
