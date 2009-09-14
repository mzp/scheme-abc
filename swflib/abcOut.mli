module type Inst = sig
  type t
  val to_bytes : t -> BytesOut.t list
end

module Make : functor (S : Inst) -> sig
  open AbcType
  val empty_cpool : cpool

  val to_bytes : S.t AbcType.t -> BytesOut.t list

  (**{6 Debug only}*)
  val of_cpool : cpool -> BytesOut.t list
  val of_method_info : method_info -> BytesOut.t list
  val of_script : script -> BytesOut.t list
  val of_trait : trait -> BytesOut.t list
  val of_method_body : S.t method_body -> BytesOut.t list

  val of_class : class_info -> BytesOut.t list
  val of_instance : instance_info -> BytesOut.t list
end
