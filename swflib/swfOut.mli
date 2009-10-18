module type TagType = sig
  type t
  val write : t -> int * SwfBaseOut.s list
end


module Make: functor (Tag:TagType) -> sig
  val write : Tag.t SwfType.t -> SwfBaseOut.t list

    (* for debug *)
  val of_tag : Tag.t -> SwfBaseOut.t list
end
