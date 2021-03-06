module type Abc = sig
  type t
  val read : int Stream.t -> t
end

module Make: functor (Abc : Abc) -> sig
  type t = Abc.t TagType.t
  val read : int -> int Stream.t -> t
end


