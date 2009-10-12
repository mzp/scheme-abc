open Base
open OUnit
open BitsIn

let ok x y =
  assert_equal x y

let _ = begin "bitsIn.ml" >::: [
  "input" >:: begin fun () ->
    let s =
      Stream.of_list [0b1011_0000; 0x1F] in
    let bs =
      BitsIn.of_stream s in
      ok 0b101 @@ bits 3 bs;
      ok 0b1   @@ bits 1 bs;
      ok 0x1F  @@ Stream.next s
  end
] end +> run_test_tt_main


