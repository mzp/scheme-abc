open Base
open AbcType

type 'a t = {
  cpool:         Cpool.t;
  method_info:   method_info list;
  method_body:   'a method_body list;
  class_info:    class_info  list;
  instance_info: instance_info list
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
  method methods: 'a method_ RevList.t
  method classes: 'a class_ RevList.t
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

let methods_map f ms =
  HList.concat_map (fun {code=code} -> f code) ms

module Make(Inst : Inst) = struct
  (* methods *)
  let methods_of_class {
    cinit=cinit;
    iinit=iinit;
    instance_methods=ims;
    static_methods=sms} = List.concat [
    [cinit;iinit];
    ims;sms
  ]

  let methods ({code=code} as m) =
    List.concat [
      [m];
      filter_map Inst.method_ code;
      HList.concat_map methods_of_class @@ filter_map Inst.class_ code;
    ]

  let classes ms =
    methods_map (filter_map Inst.class_) ms

  (* cpool *)
  let cpool ms cs =
    let entries x =
      (x :> Cpool.entry list) in
    let inst_const =
      methods_map (HList.concat_map Inst.const) ms in
    let meth_const =
      entries @@ List.map (fun {method_name=method_name} -> method_name) ms in
    let class_const =
      entries @@ HList.concat_map
	(fun {class_name=name; super=super;attrs=attrs} ->
	   name::super::attrs)
	cs in
      Cpool.add_list Cpool.empty @@ List.concat [
	inst_const;
	meth_const;
	class_const]

  let to_abc top_method =
    let ms =
      methods top_method in
    let cs =
      classes ms in
      {
	cpool=cpool ms cs;
	method_info=[];
	method_body=[];
	class_info=[];
	instance_info=[];
      }
end
