open Base
open OUnit

let _ = begin "packet.ml" >::: [
  "make" >:: begin fun () ->
    assert_equal "\005\003hello" @@ Packet.make 3 "hello"
  end;
  "parse" >:: begin fun () ->
    assert_equal (3,"hello") @@ Packet.parse "\005\003hello"
  end
] end +> run_test_tt_main

