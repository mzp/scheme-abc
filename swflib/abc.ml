open Base

module A = AbcOut.Make(LowInst)
module D = AbcIn.Make(LowInst)
module C = MethodOut.Make(HighInst)

type t = LowInst.t AbcType.t
type meth = HighInst.s MethodType.method_


let write : t -> int list =
  BytesOut.to_int_list $ A.to_bytes

let read : int Stream.t -> t =
  D.of_bytes

let compile : Cpool.multiname list -> meth -> t =
  C.to_abc
