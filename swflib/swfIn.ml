open Base
open SwfType
open SwfBaseIn

module type TagType = sig
  type t
  val read : int -> int Stream.t -> t
end


module Make(Tag:TagType) = struct
  let char n s =
    let n' =
      Char.code n in
    match Stream.peek s with
	Some m when n' = m ->
	  Stream.junk s;
	  ()
      | None | Some _ ->
	  raise Stream.Failure

  let rec many parse stream =
    match stream with parser
	[< e = parse; s>] -> e::many parse s
      | [<>] -> []

  let rec repeat n f stream =
    if n <= 0 then
      []
    else
      match stream with parser
	  [<c = f>] ->
	    c::repeat (n-1) f stream
	| [<>] ->
	    raise (Stream.Error "invalid format")

  let tag_and_size s =
    let tag_and_size =
      ui16 s in
    let tag =
      tag_and_size lsr 6 in
    let size =
      tag_and_size land 0x3f in
      if size < 0x3F then begin
	(tag,size)
      end else
	(tag, Int32.to_int @@ si32 s)

  let to_tag = parser
      [< (tag,size) = tag_and_size; body = repeat size ui8 >] ->
	Tag.read tag @@ Stream.of_list body

  let read = parser
      [< _ = char 'F'; _ = char 'W'; _ = char 'S';
	 version = ui8; _ = ui32; (left,right,top,bottom) = rect;
	 frame_rate = fixed8; frame_count = ui16; tags = many to_tag >] ->
	{
	  version;
	  frame_size  = { top; bottom; left; right };
	  frame_rate;
	  frame_count;
	  tags
	}
end
