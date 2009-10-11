open Base
open SwfType
open SwfOut
open TagOut

type swf = TagOut.t SwfType.t

module M = SwfOut.Make(TagOut)

let write ch swf =
  M.to_base swf
  +> SwfBaseOut.to_list
  +> List.iter (output_byte ch)
