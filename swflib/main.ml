open Base
open SwfType
open SwfOut
open TagOut
module M = SwfOut.Make(TagOut)
open M

let swf = {
  version     = 1;
  frame_size  = { top=0; bottom=10000; left=0; right=20000 };
  frame_rate  = 24.0;
  frame_count = 1;
  tags        = [
    `ShowFrame;
    `SetBackgroundColor {r=0xFF;g=0xFF;b=0xFF};
    `ShowFrame;
    `End;
  ]
}
let _ =
  let bytes =
    to_base swf
    +> SwfBaseOut.to_list in
    open_out_with "test.swf" begin fun ch ->
      List.iter (output_byte ch) bytes
    end
