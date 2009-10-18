open Base
open OUnit

module T = TagOut.Make(struct
			 type t = int list
			 let write x = x
		       end)

let ok x y =
  assert_equal ~printer:Std.dump x @@ T.write y

let _ = begin "tagOut.ml" >::: [
  "End" >:: begin fun () ->
    ok (0,[]) `End
  end;
  "SetBackgroundColor" >:: begin fun () ->
    ok (9,[`RGB(0,1,2)]) @@ `SetBackgroundColor (0,1,2)
  end
] end +> run_test_tt_main
