open Base
open Bytes
open Util

test u8 =
    assert_equal [50] @@ of_int_list (u8 50);
    assert_equal [0] @@ of_int_list (u8 0);
    assert_equal [0xFF] @@ of_int_list (u8 0xFF);
    assert_raises (Invalid_argument "of_int_list") @@ fun () -> of_int_list (u8 0x100)

test u16 =
    (* little endian *)
    assert_equal [0xfe;0xca] @@ of_int_list (u16 0xcafe);
    assert_equal [0;0] @@ of_int_list (u16 0);
    assert_equal [0xFF;0xFF] @@ of_int_list (u16 0xFFFF);
    assert_raises (Invalid_argument "of_int_list") @@ fun () -> of_int_list (u8 0x10000)

test s24 =
    assert_equal [0xcb;0xfe;0xca;] @@ of_int_list (s24 0xcafecb);
    assert_equal [0xfe;0xca;0] @@ of_int_list (s24 0xcafe);
    assert_equal [0;0;0] @@ of_int_list (s24 0);
    assert_equal [1;0;0] @@ of_int_list (s24 1);
    assert_equal [0xFF;0xFF;0xFF] @@ of_int_list (s24 0xFFFFFF);
    assert_equal [0xFF;0xFF;0xFF] @@ of_int_list (s24 ~-1);

test u32 =
    assert_equal [0xFF;0x30] @@ of_int_list (u30 0x187F);
    (* 1 byte/0-7bit *)
    assert_equal [0] @@ of_int_list (u30 0);
    assert_equal [0x7F] @@ of_int_list (u30 0x7F);
    (* 2 byte/8-14bit *)
    assert_equal [0xFF;0x01] @@ of_int_list (u30 0xFF);
    assert_equal [0xFF;0x7F] @@ of_int_list (u30 0x3FFF);
    (* 3 byte/15-21bit *)
    assert_equal [0xFF;0xFF;0x01] @@ of_int_list (u30 0x7FFF);
    assert_equal [0xFF;0xFF;0x7F] @@ of_int_list (u30 0x1F_FFFF);
    (* 4 byte/22-28bit *)
    assert_equal [0xFF;0xFF;0xFF;0x01] @@ of_int_list (u30 0x003F_FFFF);
    assert_equal [0xFF;0xFF;0xFF;0x7F] @@ of_int_list (u30 0x0FFF_FFFF);
    (* 5 byte/29-35bit *)
    assert_equal [0xFF;0xFF;0xFF;0xFF;0x01] @@ of_int_list (u30 0x1FFF_FFFF);
    assert_equal [0xFF;0xFF;0xFF;0xFF;0xF] @@ of_int_list (u30 0x7FFF_FFFF);

test s32 = 
  assert_equal [0x00] @@ of_int_list (s32 0);
  assert_equal [0x20] @@ of_int_list (s32 0x20);
  assert_equal [0xF6;0xFF;0xFF;0xFF;0xF] @@ of_int_list (s32 ~-10);


test label =
    let l =
      Label.make () in
      assert_equal [] @@ backpatch [];
      assert_equal [0;0xFD;0xFF;0xFF] @@ backpatch [u8 0;label l;label_ref l];
      assert_equal [0;0;0;0] @@ backpatch [u8 0;label_ref l;label l]
