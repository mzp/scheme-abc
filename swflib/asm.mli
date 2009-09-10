module type Inst = sig
  type t
  val to_bytes : t -> Bytes.t list
end

module Make : functor (S : Inst) -> sig
  open AbcType
  val empty_cpool : cpool

  val to_bytes : S.t AbcType.t -> Bytes.t list

  (**{6 Debug only}*)
  val of_cpool : cpool -> Bytes.t list
  val of_method_info : method_info -> Bytes.t list
  val of_script : script -> Bytes.t list
  val of_trait : trait -> Bytes.t list
  val of_method_body : S.t method_body -> Bytes.t list

  val of_class : class_info -> Bytes.t list
  val of_instance : instance_info -> Bytes.t list
end
