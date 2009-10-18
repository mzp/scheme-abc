module type Abc = sig
  type t
  val write : t -> int list
end

module Make: functor (Abc : Abc) -> sig
  type t = Abc.t TagType.t
  val write : t -> int * SwfBaseOut.s list
end
