
type 'a t = {
  cpool:         Cpool.t;
  method_info:   AbcType.method_info list;
  method_body:   'a AbcType.method_body list;
  class_info:    AbcType.class_info  list;
  instance_info: AbcType.instance_info list
}

type function_scope =
    [ `Global
    | `Class of Cpool.multiname]

type class_type     =
    [ `Sealed
    | `Final
    | `Interface
    | `ProtectedNs of Cpool.namespace]

type 'a method_ = {
    method_name:  Cpool.multiname;
    params:       int list;
    return:       int;
    method_flags: int;
    code:         'a list;
    traits:       int list;
    exceptions:   int list;
    fun_scope:    function_scope;
    method_attrs: [`Override | `Final] list
}

type 'a class_ = {
  class_name:       Cpool.multiname;
  super:            Cpool.multiname;
  class_flags:      class_type list;
  cinit:            'a method_;
  iinit:            'a method_;
  interface:        'a class_ list;
  instance_methods: 'a method_ list;
  static_methods:   'a method_ list;
  attrs:       Cpool.multiname list
}

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
      val to_abc : Inst.s method_ -> Inst.t t
    end
