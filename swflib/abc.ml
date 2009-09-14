module A = AbcOut.Make(LowInst)
module C = MethodOut.Make(HighInst)

type abc  = LowInst.t AbcType.t
type meth = HighInst.s MethodType.method_

let asm : abc -> BytesOut.t list                     = A.to_bytes
let compile : Cpool.multiname list -> meth -> abc = C.to_abc

