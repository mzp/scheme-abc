module A = Asm.Make(LowInst)
module C = Compile.Make(HighInst)
include AbcType
include MethodType

let asm : LowInst.t abc -> Bytes.t list = A.to_bytes
let compile : Cpool.multiname list -> HighInst.s method_ -> LowInst.t abc = C.to_abc

let output : out_channel -> Bytes.t list -> unit =
  Bytes.output_bytes


