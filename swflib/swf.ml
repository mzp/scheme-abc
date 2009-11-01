open Base
open SwfType
open SwfOut
open TagOut

type t = Abc.t TagType.t SwfType.t
exception SwfError of string

module Writer = SwfOut.Make(TagOut.Make(Abc))
module Reader = SwfIn.Make(TagIn.Make(Abc))

let write ch swf =
  Writer.write swf
  +> SwfBaseOut.to_list
  +> List.iter (output_byte ch)

let read ch =
  try
    BytesIn.of_channel ch
    +> Reader.read
  with e ->
    raise (SwfError (Printexc.to_string e))

