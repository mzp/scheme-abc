open Base
open OUnit

let ok tag xs =
  let s =
    Stream.of_list @@ SwfBaseOut.to_list xs in
    assert_equal tag (TagIn.of_base s)

let _ = begin "tagIn.ml" >::: [
  "End" >:: begin fun () ->
    ok `End [`Ui8 0]
  end;
  "SetBackgroundColor" >:: begin fun () ->
    ok (`SetBackgroundColor (0,1,2)) [`Ui8 9; `RGB(0,1,2)]
  end
] end +> run_test_tt_main
