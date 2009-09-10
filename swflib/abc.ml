open Base

include AbcType
type 'a s = 'a t

module A = Asm.Make(LowInst)

let write ch insts =
  insts
  +> A.to_bytes
  +> Bytes.output_bytes ch
