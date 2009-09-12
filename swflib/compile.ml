open Base
open AbcType
open ExtList

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
      filter_map Inst.method_ code;
      HList.concat_map methods_of_class @@ filter_map Inst.class_ code;
      [m];
    ]

  let classes ms =
    methods_map (filter_map Inst.class_) ms

  (* cpool *)
  let consts ms cs =
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

  (* methods *)
  let method_info cpool {method_name  = name;
			 params       = params;
			 return       = return;
			 method_flags = flags} =
    {
      AbcType.params = params;
      return         = return;
      method_name    = Cpool.index cpool name;
      method_flags   = flags
    }

  let method_body ctx i {params=params; code=code} =
    let max_value f xs  =
      snd @@ List.fold_left
	(fun (c,m) x ->
	   let c' = c + f x in
	     (c',max c' m))
	(0,0) xs in
      {
	AbcType.method_sig = i;
	max_stack          = max_value Inst.stack code;
	local_count        = List.length params + 1;
	init_scope_depth   = 0;
	max_scope_depth    = max_value Inst.scope code;
	code               = List.map (Inst.inst ctx) code;
	exceptions         = [];
	method_traits      = [];
      }

  let index x xs =
    fst @@ List.findi (fun _ y -> x = y) xs

  let table xs =
    fun x ->
      List.assoc x xs

  let method_trait ctx ({method_name=name; method_attrs=attrs} as m) =
    let attrs' =
     List.map (table [`Override, AbcType.ATTR_Override;
	              `Final   , AbcType.ATTR_Final])
       attrs in
    {
      AbcType.trait_name = Cpool.index ctx#cpool name;
      data               = AbcType.MethodTrait (0,index m ctx#methods,attrs')
    }

  let attr_trait ctx i name = {
    AbcType.trait_name = Cpool.index ctx#cpool name;
    data               = AbcType.SlotTrait (i+1,0,0,0)
  }

  let class_info ctx {cinit=m; static_methods=sms} = {
    AbcType.cinit = index m ctx#methods;
    class_traits  = List.map (method_trait ctx) sms;
  }

  let instance_info ctx c =
    let flag =
      function
	| `ProtectedNs ns -> AbcType.ProtectedNs (Cpool.index ctx#cpool ns)
	| _ as x ->
	    table [`Sealed,AbcType.Sealed;
		   `Final,AbcType.Final;
		   `Interface,AbcType.Interface] x in
    {
      AbcType.instance_name =
	Cpool.index ctx#cpool c.class_name;
      super_name        =
	Cpool.index ctx#cpool c.super;
      instance_flags    =
	List.map flag c.class_flags;
      interface         =
	List.map (flip index ctx#classes) c.interface;
      iinit             =
	index c.iinit ctx#methods;
      instance_traits   =
	List.concat [
	  List.map  (method_trait ctx) c.instance_methods;
	  List.mapi (attr_trait ctx)   c.attrs
	]
    }

  let to_abc top_method =
    let ms =
      methods top_method in
    let cs =
      classes ms in
    let cpool =
      consts ms cs in
    let ctx =
      {| cpool = cpool; methods = ms; classes = cs |} in
      {
	cpool         = cpool;
	method_info   = List.map  (method_info cpool) ms;
	method_body   = List.mapi (method_body ctx)   ms;
	class_info    = List.map  (class_info ctx)    cs;
	instance_info = List.map  (instance_info ctx) cs;
      }
end
