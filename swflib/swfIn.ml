open Base

module type TagType = sig
  type t
  val of_base : int -> int Stream.t -> int
end


module Make(Tag:TagType) = struct
  let of_base _ = undef
  let to_tag _ = undef
end


