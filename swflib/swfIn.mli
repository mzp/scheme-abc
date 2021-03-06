module type TagType = sig
  type t
  val read : int -> int Stream.t -> t
end


module Make: functor (Tag:TagType) -> sig
  val read : int Stream.t -> Tag.t SwfType.t

    (* for debug *)
  val to_tag : int Stream.t -> Tag.t
end
