open Base
open OUnit
open SwfBaseIn

let ok ?printer ?msg x f y =
  let s =
    Stream.of_list y in
    assert_equal ?printer ?msg x (f s);
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
    ok 0x12345678l ui32 [0x78; 0x56; 0x34; 0x12];
    ok 0xFF_FF_FF_FFl ui32 [0xFF; 0xFF; 0xFF; 0xFF]
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
  "ui64" >:: begin fun () ->
    ok ~printer:Int64.to_string 0x1L ui64 [0x1; 0; 0; 0; 0;0;0;0];
    ok ~printer:Int64.to_string 0xFF_FF_FF_FFL ui64 [0xFF; 0xFF; 0xFF; 0xFF; 0;0;0;0]
  end;
  "eui30" >:: begin fun () ->
    ok ~msg:"0" 0x0l         eui32 [0];
    ok ~msg:"7F" 0x7Fl        eui32 [0x7F];
    ok ~msg:"FF" 0xFFl        eui32 [0xFF;0x01];
    ok 0x3F_FFl     eui32 [0xFF;0x7F];
    ok 0x7FFFl      eui32 [0xFF;0xFF;0x01];
    ok 0x1F_FFFFl   eui32 [0xFF;0xFF;0x7F];
    ok 0x003F_FFFFl eui32 [0xFF;0xFF;0xFF;0x01];
    ok 0x0FFF_FFFFl eui32 [0xFF;0xFF;0xFF;0x7F];
    ok 0x1FFF_FFFFl eui32 [0xFF;0xFF;0xFF;0xFF;0x01]
  end;
  "bits" >:: begin fun () ->
    ok (1,1,1) (bits ~f:begin fun bs ->
		  let x = ub 3 bs in
		  let y = ub 1 bs in
		  let z = ub 1 bs in
		    (x,y,z)
		end)
      [0b0011_1000]
  end;
  "ub" >:: begin fun () ->
    ok 1 (bits ~f:(ub 3)) [0b0010_0000]
  end;
  "sb" >:: begin fun () ->
    ok 1 (bits ~f:(sb 3)) [0b0010_0000];
    ok ~-1 (bits ~f:(sb 3)) [0b1110_0000]
  end;
  "padding" >:: begin fun () ->
    let s =
      Stream.of_list [0b1000_0000; 0xEF] in
      assert_equal 1 (bits ~f:(ub 1) s);
      assert_equal 0xEF @@ Stream.next s
  end;
  "Fixed" >:: begin fun () ->
    ok 7.5     fixed [0x00; 0x80; 0x07; 0x00];
    ok 65535.0 fixed [0x00; 0x00; 0xFF; 0xFF]
  end;
  "Fixed8" >:: begin fun () ->
    ok 7.5     fixed8 [0x80; 0x07];
    ok 255.0   fixed8 [0x00; 0xFF]
  end;
  "Float32" >:: begin fun () ->
    ok 2.5 float32 [0; 0; 0x20; 0x40]
  end;
  "Float64" >:: begin fun() ->
    ok 0.333333333333333315 float64
      [0x55; 0x55; 0x55; 0x55;
       0x55; 0x55; 0xd5; 0x3f]
  end;
  "rect" >:: begin fun () ->
    open SwfBaseOut in begin
      ok (0,0,1,1)        rect @@ SwfBaseOut.to_list [`Bits [UB(5,2);  SB(2,0);    SB(2,0);    SB(2,1);   SB(2,1)]];
      ok (127,260,15,514) rect @@ SwfBaseOut.to_list [`Bits [UB(5,11); SB(11,127); SB(11,260); SB(11,15); SB(11,514)]]
    end
  end;
  "String" >:: begin fun () ->
    let c = Char.code in
      ok "foo" str [ c 'f'; c 'o'; c 'o'; 0];
      ok ""    str [0]
  end;
  "color" >:: begin fun () ->
    ok ~msg:"rgb"  (1,2,3)   rgb  [ 1; 2; 3 ];
    ok ~msg:"rgba" (1,2,3,4) rgba [ 1; 2; 3; 4 ];
    ok ~msg:"argb" (1,2,3,4) argb [ 1; 2; 3; 4 ]
  end
] end +> run_test_tt_main
