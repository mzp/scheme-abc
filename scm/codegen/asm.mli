module type Spec = sig
  type t
  val spec : t -> t ISpec.t
end

type t = {
  cpool:         Cpool.t;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}


module Make: functor(Spec : Spec) ->
  sig
    type method_ = Spec.t ISpec.method_
    type class_  = Spec.t ISpec.class_
    type instruction = Spec.t

    val assemble : Binding.slot list -> method_ -> Abc.abc

    val assemble_method : method_ -> t
    val assemble_slot_traits : Cpool.t -> ([< Cpool.entry ] * int) list -> Abc.trait list
  end
