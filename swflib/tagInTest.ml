open Base
open OUnit

let ok tag x xs =
  let s =
    Stream.of_list @@ SwfBaseOut.to_list xs in
    assert_equal tag (TagIn.of_base x s)

let _ = begin "tagIn.ml" >::: [
  "End" >:: begin fun () ->
    ok `End 0 []
  end;
  "SetBackgroundColor" >:: begin fun () ->
    ok (`SetBackgroundColor (0,1,2)) 9 [`RGB(0,1,2)]
  end
] end +> run_test_tt_main
