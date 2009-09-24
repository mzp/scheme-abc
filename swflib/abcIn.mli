module type Inst = sig
  type t
  val of_bytes : int Stream.t -> t
end

val cMajorVersion : int
val cMinorVersion : int


module Make : functor (S : Inst) -> sig
  open AbcType
  val of_bytes : int Stream.t -> S.t AbcType.t
end
