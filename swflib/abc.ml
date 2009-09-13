module A = Asm.Make(LowInst)
module C = Compile.Make(HighInst)
include AbcType
include MethodType

let asm = A.to_bytes
let compile = C.to_abc
