open MethodType

class type ['a] context = object
  method cpool:   Cpool.t
  method methods: 'a method_ list
  method classes: 'a class_ list
end

module type Inst = sig
  type s (* source *)
  type t (* target *)

  val inst  : s context -> s -> t
  val const : s -> Cpool.entry list
  val stack : s -> int
  val scope : s -> int
  val method_ : s -> s method_ option
  val class_ : s -> s class_ option
end

module Make :
  functor (Inst : Inst) ->
    sig
      val to_abc     : Cpool.multiname list -> Inst.s method_ -> Inst.t AbcType.abc
      val __to_cpool : Inst.s method_ -> Cpool.t
    end
