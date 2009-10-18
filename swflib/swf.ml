open Base
open SwfType
open SwfOut
open TagOut

type t = LowInst.t TagType.t SwfType.t

module Writer = SwfOut.Make(TagOut.Make(Abc))
module Reader = SwfIn.Make(TagIn.Make(Abc))

let write ch swf =
  Writer.write swf
  +> SwfBaseOut.to_list
  +> List.iter (output_byte ch)

let read ch =
  BytesIn.of_channel ch
  +> Reader.read

