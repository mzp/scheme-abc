module type Inst = sig
  type t
  val of_bytes : BytesIn.t Stream.t -> t
end

val cMajorVersion : int
val cMinorVersion : int


module Make : functor (S : Inst) -> sig
  open AbcType
  val of_bytes : BytesIn.t Stream.t -> S.t AbcType.t

  (**{6 Debug only}*)
  val to_cpool : BytesIn.t Stream.t -> cpool
  val to_method_info : BytesIn.t Stream.t -> method_info
  val to_metadata : BytesIn.t Stream.t -> metadata
  val to_trait : BytesIn.t Stream.t -> trait

  val to_script : BytesIn.t Stream.t -> script

  val to_method_body : BytesIn.t Stream.t -> S.t method_body

  val to_class : BytesIn.t Stream.t -> class_info
  val to_instance : BytesIn.t Stream.t -> instance_info
end
