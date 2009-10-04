open Base
open SwfType

let char c =
  `Ui8 (Char.code c)

let of_rect {top; bottom; left; right} =
  `Rect(left,right,top,bottom)

let to_base t = [
  (* signature *)
  char 'F'; char 'W'; char 'S';
  (* version *)
  `Ui8 t.version;
  (* file length *)
  `Ui32Size;
  (* frame size *)
  of_rect t.frame_size;
  (* frame rate *)
  `Fixed8 t.frame_rate;
  (* frame count *)
  `Ui16 t.frame_count
]

