open Base
open OUnit

module T = TagIn.Make(
  struct
    type t = int
    let read _ = 0
  end)

let ok tag x xs =
  let s =
    Stream.of_list @@ SwfBaseOut.to_list xs in
    assert_equal tag (T.read x s)

let _ = begin "tagIn.ml" >::: [
  "End" >:: begin fun () ->
    ok `End 0 []
  end;
  "SetBackgroundColor" >:: begin fun () ->
    ok (`SetBackgroundColor (0,1,2)) 9 [`RGB(0,1,2)]
  end;
  "FrameLabel" >:: begin fun ()->
    ok (`FrameLabel ("foo",false)) 43 [`Str "foo"];
    ok (`FrameLabel ("foo",true))  43 [`Str "foo"; `Ui8 1]
  end;
  "FileAttributes" >:: begin fun () ->
    open SwfBaseOut in
    ok (`FileAttributes { TagType.is_metadata=true; is_as3=true; use_network=true}) 69 [
      `Bits [ UB(3,0); UB(1,1); UB(1,1); UB(2,0); UB(1,1); UB(24,0)]
    ]
  end
] end +> run_test_tt_main
