module type TagType = sig
  type t
  val to_base : t -> int * SwfBaseOut.t list
end


module Make: functor (Tag:TagType) -> sig
  val to_base : Tag.t SwfType.t -> SwfBaseOut.t list

    (* for debug *)
  val of_tag : Tag.t -> SwfBaseOut.t list
end
