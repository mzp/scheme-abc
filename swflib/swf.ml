open Base
open SwfType
open SwfOut
open TagOut

type swf = TagType.t SwfType.t

module M = SwfOut.Make(struct
			 type t = TagType.t
			 let to_base = TagOut.to_base
		       end)

let write ch swf =
  M.to_base swf
  +> SwfBaseOut.to_list
  +> List.iter (output_byte ch)
