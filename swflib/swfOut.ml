open Base
open SwfType

let char c =
  `Ui8 (Char.code c)

let of_rect {top; bottom; left; right} =
  `Rect(left,right,top,bottom)

let of_tag {tag; data} =
  let make_type t size =
    `Ui16 ((t lsl 6) lor size) in
  let size =
    List.length data in
    if size < 0x3F then
      make_type tag size :: data
    else
      make_type tag 0x3F :: `Si32 (Int32.of_int size) :: data

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

