open Base
open SwfType
open SwfOut
open TagOut

type swf = TagType.t SwfType.t

module Writer = SwfOut.Make(struct
			      type t = TagType.t
			      include TagOut
			    end)

module Reader = SwfIn.Make(struct
			     type t = TagType.t
			     include TagIn
			   end)

let write ch swf =
  Writer.to_base swf
  +> SwfBaseOut.to_list
  +> List.iter (output_byte ch)
