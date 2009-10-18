open Base
open SwfType
open SwfOut
open TagOut

let swf = {
  version     = 1;
  frame_size  = { SwfType.top=0; bottom=10000; left=0; right=20000 };
  frame_rate  = 24.0;
  frame_count = 1;
  tags        = [
    `PlaceObject (1,0,{SwfBaseOut.translate=(1,2); scale=None; rotate=None});
    `ShowFrame;
    `SetBackgroundColor (0x0,0x0,0xFF);
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
