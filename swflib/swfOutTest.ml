open Base
open SwfType
open SwfOut
open OUnit

let char c =
  `Ui8 (Char.code c)

let ok x y =
  assert_equal x @@ to_base y

let _ = begin "swfOut.ml" >::: [
  "header" >:: begin fun () ->
    let swf = {
      version     = 9;
      frame_size  = { top=0; bottom=10; left=0; right=20 };
      frame_rate  = 24.0;
      frame_count = 42;
    } in
      ok [
	(* signature *)
	char 'F'; char 'W'; char 'S';
	(* version *)
	`Ui8 9;
	(* file length *)
	`Ui32Size;
	(* frame size *)
	`Rect(0,20,0,10);
	(* frame rate *)
	`Fixed8 24.0;
	(* frame count *)
	`Ui16 42;
      ] swf
  end
] end +> run_test_tt_main
