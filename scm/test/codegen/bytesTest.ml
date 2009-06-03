open Base
open OUnit
open Bytes

let encode x =
  Bytes.to_int_list [x]

let ok expect xs =
  assert_equal ~printer:Std.dump expect @@
    to_int_list xs

let same ?msg expect actual =
  assert_equal ?msg (to_int_list expect) (to_int_list actual)

let _ =
  ("bytes.ml" >::: [
     "u8" >::
       (fun () ->
	  ok [50]   [u8 50];
	  ok [0]    [u8 0];
	  ok [0xFF] [u8 0xFF];
	  assert_raises Out_of_range
	    (fun () -> u8 0x100);
	  assert_raises Out_of_range
	    (fun () -> u8 ~-1));
     "u16" >::
       (fun () ->
	  (* memo: little endian *)
	  ok [0xfe;0xca] [u16 0xcafe];
	  ok [0;0]       [u16 0];
	  ok [0xFF;0xFF] [u16 0xFFFF];
	  assert_raises Out_of_range
	    (fun () -> u16 0x10000);
	  assert_raises Out_of_range
	    (fun () -> u16 ~-1));
     "s24" >::
       (fun () ->
	  ok [0xcb;0xfe;0xca] [s24 0xcafecb];
	  ok [0xfe;0xca;0]    [s24 0xcafe];
	  ok [0;0;0]          [s24 0];
	  ok [1;0;0]          [s24 1];
	  ok [0xFF;0xFF;0xFF] [s24 0xFFFFFF];
	  ok [0xFF;0xFF;0xFF] [s24 ~-1]);
     "u32" >::: [
       "1byte" >::
	 (fun () ->
	    ok [0]    [u30 0];
	    ok [0x7F] [u30 0x7F]);
       "2byte" >::
	 (fun () ->
	    ok [0xFF;0x30] [u30 0x187F];
	    ok [0xFF;0x01] [u30 0xFF];
	    ok [0xFF;0x7F] [u30 0x3FFF]);
       "3byte/15-21bit" >::
	 (fun () ->
	    ok [0xFF;0xFF;0x01] [u30 0x7FFF];
	    ok [0xFF;0xFF;0x7F] [u30 0x1F_FFFF]);
       "4 byte/22-28bit" >::
	 (fun () ->
	    ok [0xFF;0xFF;0xFF;0x01] [u30 0x003F_FFFF];
	    ok [0xFF;0xFF;0xFF;0x7F] [u30 0x0FFF_FFFF]);
       "5 byte/29-35bit" >::
	 (fun () ->
	    ok [0xFF;0xFF;0xFF;0xFF;0x01] [u30 0x1FFF_FFFF];
	    ok [0xFF;0xFF;0xFF;0xFF;0xF]  [u30 0x7FFF_FFFF])];
     "s32" >::
       (fun () ->
	  ok [0x00] [s32 0];
	  ok [0x20] [s32 0x20];
	  ok [0xF6;0xFF;0xFF;0xFF;0xF] [s32 ~-10]);
     "d64" >::
       (fun () ->
	  ok [0;0;0;0;0;0;0xe8;0x3f] [d64 0.75]);
     "label" >::
       (fun () ->
	  let l =
	    Label.make () in
	    same ~msg:"back" [s24 ~-3] [label l;label_ref l];
	    same ~msg:"none" [s24 0]         [label_ref l;label l];
	    same ~msg:"u8"   [s24 1; u8 1]   [label_ref l;u8 1;label l];
	    same ~msg:"s24"  [s24 3; s24 1]  [label_ref l;s24 1;label l]
       );
   ]) +> run_test_tt

