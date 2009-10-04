open Base
open OUnit
open SwfBaseOut

let ok_i x y =
  assert_equal ~printer:Std.dump x @@ to_int_list [ y ]

let ok_b x y =
  assert_equal ~printer:Std.dump (to_int_list x) (to_int_list y)

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
    ok_b [Ui16 0x8000; Ui16 7] [Fixed 7.5];
    ok_b [Ui16 0; Ui16 0xFFFF] [Fixed 65535.0];
  end;
  "Fixed8" >:: begin fun () ->
    ok_b [Ui8 0x80; Ui8 7] [Fixed8 7.5];
    ok_b [Ui8 0; Ui8 0xFF] [Fixed8 255.0];
  end;
(*  "Float16" >:: begin fun() ->
    ok_i [0; 0x3c] @@ Float16 1.;
    ok_i [0; 0xc0] @@ Float16 (-2.0);
    ok_i [0xff; 0x7b] @@ Float16 65504.;
  end;*)
  "Float32" >:: begin fun() ->
    ok_i [0; 0; 0x20; 0x40] @@ Float32 2.5;
  end;
  "Float64" >:: begin fun() ->
    ok_i [0x55; 0x55; 0x55; 0x55;
	  0x55; 0x55; 0xd5; 0x3f] @@ Float64 0.333333333333333315;
  end;
  "u30" >::: [
    "1byte" >:: begin fun () ->
      ok_i [0]    @@ EUi32 0l;
      ok_i [0x7F] @@ EUi32 0x7Fl
    end;
    "2byte" >:: begin fun () ->
      ok_i [0xFF;0x30] @@ EUi32 0x187Fl;
      ok_i [0xFF;0x01] @@ EUi32 0xFFl;
      ok_i [0xFF;0x7F] @@ EUi32 0x3FFFl
    end;
    "3byte/15-21bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0x01] @@ EUi32 0x7FFFl;
      ok_i [0xFF;0xFF;0x7F] @@ EUi32 0x1F_FFFFl
    end;
    "4 byte/22-28bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0xFF;0x01] @@ EUi32 0x003F_FFFFl;
      ok_i [0xFF;0xFF;0xFF;0x7F] @@ EUi32 0x0FFF_FFFFl
    end;
    "5 byte/29-35bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0xFF;0xFF;0x01] @@ EUi32 0x1FFF_FFFFl;
      ok_i [0xFF;0xFF;0xFF;0xFF;0x03] @@ EUi32 0x3FFF_FFFFl
    end
  ];
] end +> run_test_tt_main
