open Base
open SwfType

module type TagType = sig
  type t
  val to_base : t -> int * SwfBaseOut.t list
end

module Make(Tag : TagType) = struct
  let char c =
    `Ui8 (Char.code c)

  let of_rect {top; bottom; left; right} =
    `Rect(left,right,top,bottom)

  let of_tag tag =
    let make_type t size =
      `Ui16 ((t lsl 6) lor size) in
    let tag,data' =
      Tag.to_base tag in
    let size =
      List.length @@ SwfBaseOut.to_list data' in
      if size < 0x3F then
	make_type tag size :: data'
      else
	make_type tag 0x3F :: `Si32 (Int32.of_int size) :: data'

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
  ] @ HList.concat_map of_tag t.tags

end
