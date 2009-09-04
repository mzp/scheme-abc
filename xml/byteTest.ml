open OUnit
open Base
open Byte
open TestSupport

let of_list xs =
  Stream.of_list xs

let ok x y =
  OUnit.assert_equal ~printer:Std.dump x y

let tests = ("byte.ml" >::: [
  "u8 is single byte" >::
    (fun _ ->
       ok 0 (u8 @@ of_list [0]));
  "u16 is little endian" >::
    (fun _ ->
       ok 0x0100 (u16 @@ of_list [0;1] ));
  "s24" >::
    (fun _ ->
       ok 0x000001 (s24 @@ of_list [1;0;0] );
       ok ~-1 (s24 @@ of_list [0xFF;0xFF;0xFF] ));
  "u30 is single byte when value < 0x7F" >::
    (fun _ ->
       ok 0l    (u30 @@ of_list [0]);
       ok 0x7Fl (u30 @@ of_list [0x7F]));
  "u30 is 2 bytes when value <= 0x7F 0xFF"  >::
    (fun _ ->
       ok 0xFFl    (u30 @@ of_list [0xFF;0x01]);
       ok 0x3F_FFl (u30 @@ of_list [0xFF;0x7F]));
  "u30 is 3 bytes when value <= 0x7F 0xFF 0xFF" >::
    (fun _ ->
       ok 0x7FFFl    (u30 @@ of_list [0xFF;0xFF;0x01]);
       ok 0x1F_FFFFl (u30 @@ of_list [0xFF;0xFF;0x7F]));
  "u30 is 4 bytes when value <= 0x7F 0xFF 0xFF 0xFF" >::
    (fun _ ->
       ok 0x003F_FFFFl (u30 @@ of_list [0xFF;0xFF;0xFF;0x01]);
       ok 0x0FFF_FFFFl (u30 @@ of_list [0xFF;0xFF;0xFF;0x7F]));
  "u30 is 5 bytes when value <= 0x7F 0xFF 0xFF 0xFF 0xFF" >::
    (fun _ ->
       ok 0x1FFF_FFFFl (u30 @@ of_list [0xFF;0xFF;0xFF;0xFF;0x01]);
       ok 0xFFFF_FFFFl (u30 @@ of_list [0xFF;0xFF;0xFF;0xFF;0xF]));
  "d64 is float(IEEE 754 format)" >::
    (fun _ ->
       ok 0.75 (d64 @@ of_list [0;0;0;0;0;0;0xe8;0x3f]))
]) +> run_test_tt_main
