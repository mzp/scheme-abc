open Base
open SwfType
open SwfIn
open OUnit

let rec entire s =
  try
    let x =
      Stream.next s in
    let xs =
      entire s in
      x::xs
  with Stream.Failure ->
    []

module M = SwfIn.Make(
  struct
    type t = int * int list
    let read t s = (t,entire s)
  end)
open M

let char c =
  `Ui8 (Char.code c)

let ok ?msg x f y =
  assert_equal ~printer:Std.dump ?msg x (f @@ Stream.of_list @@ SwfBaseOut.to_list y)

let ok_b ?msg f x y =
  assert_equal ?msg (SwfBaseOut.to_list y) (SwfBaseOut.to_list (f x))

let _ = begin "swfIn.ml" >::: [
  "compress" >:: begin fun () ->
    open_in_with "compress.swf" begin fun ch ->
      ignore @@ M.read @@ BytesIn.of_channel ch
    end
  end;
  "header" >:: begin fun () ->
    let swf = {
      version     = 9;
      frame_size  = { top=0; bottom=10; left=0; right=20 };
      frame_rate  = 24.0;
      frame_count = 42;
      tags        = []
    } in
      ok swf M.read [
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
      ]
  end;
  "tag" >:: begin fun () ->
    ok ~msg:"size < 64" (1, [1; 2; 3]) M.to_tag
      [ `Ui16 0b0000000001_000011; `Ui8 1; `Ui8 2; `Ui8 3 ];
    (* size >= 64*)
    ok ~msg:"size > 64" (1,HList.replicate 64 1) M.to_tag @@
      [ `Ui16 0b0000000001_111111; `Si32 64l ] @ HList.replicate 64 (`Ui8 1)
  end
] end +> run_test_tt_main
