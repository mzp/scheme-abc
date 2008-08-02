open Base
open Bytes
open Util

let encode x =
  Bytes.to_int_list [x]

test u8 =
    assert_equal [50] @@ encode (u8 50);
    assert_equal [0] @@ encode (u8 0);
    assert_equal [0xFF] @@ encode (u8 0xFF);
    assert_raises (Invalid_argument "Bytes.u8") @@ fun () -> u8 0x100;
    assert_raises (Invalid_argument "Bytes.u8") @@ fun () -> u8 ~-1

test u16 =
    (* little endian *)
    assert_equal [0xfe;0xca] @@ encode (u16 0xcafe);
    assert_equal [0;0] @@ encode (u16 0);
    assert_equal [0xFF;0xFF] @@ encode (u16 0xFFFF);
    assert_raises (Invalid_argument "Bytes.u16") @@ fun () -> u16 0x10000;
    assert_raises (Invalid_argument "Bytes.u16") @@ fun () -> u16 ~-1

test s24 =
    assert_equal [0xcb;0xfe;0xca;] @@ encode (s24 0xcafecb);
    assert_equal [0xfe;0xca;0] @@ encode (s24 0xcafe);
    assert_equal [0;0;0] @@ encode (s24 0);
    assert_equal [1;0;0] @@ encode (s24 1);
    assert_equal [0xFF;0xFF;0xFF] @@ encode (s24 0xFFFFFF);
    assert_equal [0xFF;0xFF;0xFF] @@ encode (s24 ~-1);

test u32 =
    assert_equal [0xFF;0x30] @@ encode (u30 0x187F);
    (* 1 byte/0-7bit *)
    assert_equal [0] @@ encode (u30 0);
    assert_equal [0x7F] @@ encode (u30 0x7F);
    (* 2 byte/8-14bit *)
    assert_equal [0xFF;0x01] @@ encode (u30 0xFF);
    assert_equal [0xFF;0x7F] @@ encode (u30 0x3FFF);
    (* 3 byte/15-21bit *)
    assert_equal [0xFF;0xFF;0x01] @@ encode (u30 0x7FFF);
    assert_equal [0xFF;0xFF;0x7F] @@ encode (u30 0x1F_FFFF);
    (* 4 byte/22-28bit *)
    assert_equal [0xFF;0xFF;0xFF;0x01] @@ encode (u30 0x003F_FFFF);
    assert_equal [0xFF;0xFF;0xFF;0x7F] @@ encode (u30 0x0FFF_FFFF);
    (* 5 byte/29-35bit *)
    assert_equal [0xFF;0xFF;0xFF;0xFF;0x01] @@ encode (u30 0x1FFF_FFFF);
    assert_equal [0xFF;0xFF;0xFF;0xFF;0xF] @@ encode (u30 0x7FFF_FFFF);

test s32 = 
  assert_equal [0x00] @@ encode (s32 0);
  assert_equal [0x20] @@ encode (s32 0x20);
  assert_equal [0xF6;0xFF;0xFF;0xFF;0xF] @@ encode (s32 ~-10);

test d64 =
  assert_equal [0;0;0;0;0;0;0xe8;0x3f] @@ encode (d64 0.75)

test label =
    let l =
      Label.make () in
      assert_equal [] @@ to_int_list [];
      assert_equal [0;0xFD;0xFF;0xFF] @@ to_int_list [u8 0;label l;label_ref l];
      assert_equal [0;0;0;0] @@ to_int_list [u8 0;label_ref l;label l];
      assert_equal [42;1;0;0;43] @@ to_int_list [u8 42;label_ref l;u8 43;label l]

test block =
    let bytes = 
      to_int_list [block [u8 0; u30 0xFF]] in
      assert_equal [3; 0; 0xFF;0x01] bytes

