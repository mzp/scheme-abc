open Base
open OUnit
open SwfType
open SwfBaseOut

let char c =
  `Ui8 (Char.code c)

let ok_i x y =
  assert_equal ~printer:Std.dump x @@ to_list [ y ]

let ok_b x y =
  assert_equal ~printer:Std.dump (to_list x) (to_list y)

let _ = begin "swfBaseOut.ml" >::: [
  "equality" >:: begin fun () ->
    assert_equal (`Si8 0) (`Si8 0)
  end;
  "`Si8" >:: begin fun () ->
    ok_i  [0x7F] @@ `Si8 0x7F;
    ok_i  [0xFF] @@ `Si8 (~-1);
  end;
  "`Si16" >:: begin fun () ->
    ok_i [0x7F; 0x00] @@ `Si16 0x7F;
    ok_i [0xFF; 0x7F] @@ `Si16 0x7FFF;
    ok_i [0xFF; 0xFF] @@ `Si16 (~-1);
  end;
  "`Si24" >:: begin fun () ->
    ok_i [0x7F; 0; 0] @@ `Si24 0x7F;
    ok_i [0x56; 0x34; 0x12] @@ `Si24 0x123456;
    ok_i [0xFF; 0xFF; 0xFF] @@ `Si24 (~-1);
  end;
  "`Si32" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0x7F] @@ `Si32 0x7FFFFFFFl;
    ok_i [0xFF; 0xFF; 0xFF; 0xFF] @@ `Si32 (-1l);
  end;
  "`Ui8" >:: begin fun () ->
    ok_i [0x0] @@ `Ui8 0x0;
    ok_i [0xFF] @@ `Ui8 0xFF;
  end;
  "`Ui16" >:: begin fun () ->
    ok_i [0x00; 0x00] @@ `Ui16 0;
    ok_i [0xFF; 0xFF] @@ `Ui16 0xFFFF;
    ok_i [0x34; 0x12] @@ `Ui16 0x1234;
  end;
  "`Ui24" >:: begin fun () ->
    ok_i [0;0;0] @@ `Ui24 0;
    ok_i [0x56; 0x34; 0x12] @@ `Ui24 0x123456;
    ok_i [0xFF; 0xFF; 0xFF] @@ `Ui24 0xFFFFFF;
  end;
  "`Ui32" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0x7F] @@ `Ui32 0x7FFFFFFFl;
    ok_i [0xFF; 0xFF; 0xFF; 0xFF] @@ `Ui32 0xFFFFFFFFl;
  end;
  "`Ui64" >:: begin fun () ->
    ok_i [0xFF; 0xFF; 0xFF; 0xFF; 0;0;0;0] @@
      `Ui64 0xFFFFFFFFL;
  end;
  "Fixed" >:: begin fun () ->
    ok_b [`Ui16 0x8000; `Ui16 7] [`Fixed 7.5];
    ok_b [`Ui16 0; `Ui16 0xFFFF] [`Fixed 65535.0];
  end;
  "Fixed8" >:: begin fun () ->
    ok_b [`Ui8 0x80; `Ui8 7] [`Fixed8 7.5];
    ok_b [`Ui8 0; `Ui8 0xFF] [`Fixed8 255.0];
  end;
(*  "Float16" >:: begin fun() ->
    ok_i [0; 0x3c] @@ Float16 1.;
    ok_i [0; 0xc0] @@ Float16 (-2.0);
    ok_i [0xff; 0x7b] @@ Float16 65504.;
  end;*)
  "Float32" >:: begin fun() ->
    ok_i [0; 0; 0x20; 0x40] @@ `Float32 2.5;
  end;
  "Float64" >:: begin fun() ->
    ok_i [0x55; 0x55; 0x55; 0x55;
	  0x55; 0x55; 0xd5; 0x3f] @@ `Float64 0.333333333333333315;
  end;
  "u30" >::: [
    "1byte" >:: begin fun () ->
      ok_i [0]    @@ `EUi32 0l;
      ok_i [0x7F] @@ `EUi32 0x7Fl
    end;
    "2byte" >:: begin fun () ->
      ok_i [0xFF;0x30] @@ `EUi32 0x187Fl;
      ok_i [0xFF;0x01] @@ `EUi32 0xFFl;
      ok_i [0xFF;0x7F] @@ `EUi32 0x3FFFl
    end;
    "3byte/15-21bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0x01] @@ `EUi32 0x7FFFl;
      ok_i [0xFF;0xFF;0x7F] @@ `EUi32 0x1F_FFFFl
    end;
    "4 byte/22-28bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0xFF;0x01] @@ `EUi32 0x003F_FFFFl;
      ok_i [0xFF;0xFF;0xFF;0x7F] @@ `EUi32 0x0FFF_FFFFl
    end;
    "5 byte/29-35bit" >:: begin fun () ->
      ok_i [0xFF;0xFF;0xFF;0xFF;0x01] @@ `EUi32 0x1FFF_FFFFl;
      ok_i [0xFF;0xFF;0xFF;0xFF;0x03] @@ `EUi32 0x3FFF_FFFFl
    end
  ];
  "UB" >:: begin fun () ->
    ok_i [0b00001_000] @@ `Bits [UB (5,1)];
  end;
  "SB" >:: begin fun () ->
    ok_i [0b00001_000] @@ `Bits [SB (5,1)];
    ok_i [0b11111_000] @@ `Bits [SB (5,-1)];
  end;
  "FB" >:: begin fun () ->
    ok_i [0x00;0x00;0b0110_0000] @@ `Bits [FB (19,3.0)]
  end;
  "bits padding" >:: begin fun () ->
    ok_i [0b00001_000; 0b1000_0000] @@ `Bits [UB (5,1); UB (4,1)]
  end;
  "rect" >:: begin fun () ->
    ok_b [ `Bits [UB(5,2);  SB(2,0);    SB(2,0);    SB(2,1);   SB(2,1)]]    @@ [`Rect (0,0,1,1)];
    ok_b [ `Bits [UB(5,11); SB(11,127); SB(11,260); SB(11,15); SB(11,514)]] @@ [`Rect (127,260,15,514)]
  end;
  "String" >:: begin fun () ->
    ok_b [ char 'f'; char 'o'; char 'o'; `Ui8 0] @@ [`Str "foo"];
    ok_b [ `Ui8 0] @@ [`Str ""]
  end;
  "whole size" >:: begin fun () ->
    ok_b [`Ui32 4l] [`Ui32Size];
    ok_b [`Ui32 6l; `EUi32 0xFFl] [`Ui32Size;  `EUi32 0xFFl;];
  end;
  "size" >:: begin fun () ->
    ok_b [`Ui8 2; `Ui8 0; `Ui8 0]   [`Size ((fun n -> [`Ui8 n]),[`Ui8 0; `Ui8 0])];
    ok_b [`Ui32 6l; `Ui8 0; `Ui8 0] [`Ui32Size; `Size ((fun _ -> [`Ui8 0;`Ui8 0]),[])];
    ok_b [`Ui8 0;`Ui32 7l; `Ui8 0; `Ui8 0] [`Ui8 0; `Ui32Size; `Size ((fun _ -> [`Ui8 0;`Ui8 0]),[])];
    ok_b [`Ui32 8l; `Ui8 0; `Ui8 0;`Ui8 0;`Ui8 0] [`Ui32Size;
						   `Size ((fun _ -> [`Ui8 0;`Ui8 0]),[]);
						   `Size ((fun _ -> [`Ui8 0;`Ui8 0]),[])]
  end;
  "rgb" >:: begin fun () ->
    ok_b [`Ui8 1; `Ui8 2; `Ui8 3] [`RGB (1,2,3)];
    ok_b [`Ui8 1; `Ui8 2; `Ui8 3; `Ui8 4] [`RGBA (1,2,3,4)];
    ok_b [`Ui8 1; `Ui8 2; `Ui8 3; `Ui8 4] [`ARGB (1,2,3,4)];
  end;
  "lang" >:: begin fun () ->
    ok_b [`Ui8 0] [`Lang 0];
    ok_b [`Ui8 1] [`Lang 1];
  end;
  "matrix" >:: begin fun () ->
    ok_b [`Bits [UB(1,0); UB(1,0); UB(5,3); SB(3,1); SB(3,2)]] [`Matrix {scale=None; rotate=None; translate=(1,2)}]
  end;
(*  "cxform" >:: begin fun () ->
    ok_b [`Bits [UB(1,0); UB(1,0); UB(4,0)]]
      [`Cxform (None, None)];
    ok_b [`Bits [UB(1,1); UB(1,0); UB(4,0)]]
      [`Cxform (Some{red=1; green=2; blue=3}, None)];
    ok_b [`Bits [UB(1,0); UB(1,1); UB(4,0)]]
      [`Cxform (None, Some{red=1; green=2; blue=3})];
    ok_b [`Bits [UB(1,1); UB(1,1); UB(4,0)]]
      [`Cxform (Some{red=1; green=2; blue=3}, Some{red=4; green=5; blue=6})];
  end*)
] end +> run_test_tt_main
