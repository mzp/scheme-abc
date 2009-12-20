open Base
open Swflib
open Swflib.SwfType

let make t abc : Swf.t = {
  version     = 9;
  frame_count = 1;
  frame_rate  = 24.;
  frame_size  = {
    left   = 0;
    top    = 0;
    right  = fst t#size;
    bottom = snd t#size;
  };
  tags = [
    `FileAttributes { TagType.is_as3=true;  is_metadata=true; use_network=t#use_network };
    `SetBackgroundColor t#color;
    `FrameLabel (t#main_class,false);
    `DoABC(true,"frame",abc);
    `SymbolClass [(0, t#main_class)];
    `ShowFrame;
    `End
  ]
}
