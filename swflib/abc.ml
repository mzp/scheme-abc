module A = Asm.Make(LowInst)
module C = Compile.Make(HighInst)

type abc  = LowInst.t AbcType.t
type meth = HighInst.s MethodType.method_

let asm : abc -> Bytes.t list                     = A.to_bytes
let compile : Cpool.multiname list -> meth -> abc = C.to_abc
let output : out_channel -> Bytes.t list -> unit  = Bytes.output_bytes
