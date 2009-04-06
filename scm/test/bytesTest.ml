open Base
open Bytes
open Util
open OUnit

let encode x =
  Bytes.to_int_list [x]

let _ =
  ("byte module test" >::: [
     "u8" >::
       (fun () ->
	  ok [50] @@ encode (u8 50);
	  ok [0] @@ encode (u8 0);
	  ok [0xFF] @@ encode (u8 0xFF);
	  assert_raises (Invalid_argument "Bytes.u8: 256")
	    (fun () -> u8 0x100);
	  assert_raises (Invalid_argument "Bytes.u8: -1")
	    (fun () -> u8 ~-1));
     "u16" >::
       (fun () ->
	  (* little endian *)
	  ok [0xfe;0xca] @@ encode (u16 0xcafe);
	  ok [0;0] @@ encode (u16 0);
	  ok [0xFF;0xFF] @@ encode (u16 0xFFFF);
	  assert_raises (Invalid_argument "Bytes.u16")
	    (fun () -> u16 0x10000);
	  assert_raises (Invalid_argument "Bytes.u16")
	    (fun () -> u16 ~-1));
     "s24" >::
       (fun () ->
	  ok [0xcb;0xfe;0xca;] @@ encode (s24 0xcafecb);
	  ok [0xfe;0xca;0] @@ encode (s24 0xcafe);
	  ok [0;0;0] @@ encode (s24 0);
	  ok [1;0;0] @@ encode (s24 1);
	  ok [0xFF;0xFF;0xFF] @@ encode (s24 0xFFFFFF);
	  ok [0xFF;0xFF;0xFF] @@ encode (s24 ~-1));
     "u32" >::
       (fun () ->
	  ok [0xFF;0x30] @@ encode (u30 0x187F);
	  (* 1 byte/0-7bit *)
	  ok [0] @@ encode (u30 0);
	  ok [0x7F] @@ encode (u30 0x7F);
	  (* 2 byte/8-14bit *)
	  ok [0xFF;0x01] @@ encode (u30 0xFF);
	  ok [0xFF;0x7F] @@ encode (u30 0x3FFF);
	  (* 3 byte/15-21bit *)
	  ok [0xFF;0xFF;0x01] @@ encode (u30 0x7FFF);
	  ok [0xFF;0xFF;0x7F] @@ encode (u30 0x1F_FFFF);
	  (* 4 byte/22-28bit *)
	  ok [0xFF;0xFF;0xFF;0x01] @@ encode (u30 0x003F_FFFF);
	  ok [0xFF;0xFF;0xFF;0x7F] @@ encode (u30 0x0FFF_FFFF);
	  (* 5 byte/29-35bit *)
	  ok [0xFF;0xFF;0xFF;0xFF;0x01] @@ encode (u30 0x1FFF_FFFF);
	  ok [0xFF;0xFF;0xFF;0xFF;0xF] @@ encode (u30 0x7FFF_FFFF));
     "s32" >::
       (fun () ->
	  ok [0x00] @@ encode (s32 0);
	  ok [0x20] @@ encode (s32 0x20);
	  ok [0xF6;0xFF;0xFF;0xFF;0xF] @@ encode (s32 ~-10));
     "d64" >::
       (fun () ->
	  ok [0;0;0;0;0;0;0xe8;0x3f] @@ encode (d64 0.75));
     "label" >::
       (fun () ->
	  let l =
	    Label.make () in
	    ok [] @@ to_int_list [];
	    ok [0;0xFD;0xFF;0xFF] @@ to_int_list [u8 0;label l;label_ref l];
	    ok [0;0;0;0] @@ to_int_list [u8 0;label_ref l;label l];
	    ok [42;1;0;0;43] @@ to_int_list [u8 42;label_ref l;u8 43;label l]);
     "block" >::
       (fun () ->
	  ok [3; 0; 0xFF;0x01] @@ to_int_list [block [u8 0; u30 0xFF]])
   ]) +> run_test_tt

