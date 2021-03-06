open Base
open AbcType
open ExtList
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

let methods_map f ms =
  HList.concat_map (fun {code=code} -> f code) ms

module Make(Inst : Inst) = struct
  (* methods *)
  let methods_of_class { cinit; iinit;
			 instance_methods=ims;
			 static_methods=sms} =
    List.concat [ [cinit;iinit];
		  ims;sms ]

  let rec methods ({code=code} as m) =
    let ms =
      filter_map Inst.method_ code in
    let cms =
      HList.concat_map methods_of_class @@ filter_map Inst.class_ code in
      HList.concat_map methods ms @ cms @ [ m ]

  let classes =
    methods_map @@ filter_map Inst.class_

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
			 params;return;
			 method_flags} =
    {
      AbcType.params = params;
      return;
      method_name    = Cpool.index cpool name;
      method_flags;
    }

  let method_body ctx i {params; code} =
    let max_value f xs  =
      snd @@ List.fold_left
	(fun (c,m) x -> (c + f x,max m @@ c + (f x))) (0,0) xs in
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

  let table xs x=
    List.assoc x xs

  let method_trait ctx ({method_name=name; method_attrs=attrs} as m) =
    let attrs' =
     List.map (table [`Override, AbcType.ATTR_Override;
	              `Final   , AbcType.ATTR_Final])
       attrs in
    {
      AbcType.trait_name = Cpool.index ctx#cpool name;
      data               = AbcType.MethodTrait (0,index m ctx#methods,attrs');
      trait_metadata     = []
    }

  let attr_trait ctx i name = {
    AbcType.trait_name = Cpool.index ctx#cpool name;
    data               = AbcType.SlotTrait (i+1,0,0,0);
    trait_metadata     = []
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
      interfaces        =
	List.map (flip index ctx#classes) c.interface;
      iinit             =
	index c.iinit ctx#methods;
      instance_traits   =
	List.concat [
	  List.map  (method_trait ctx) c.instance_methods;
	  List.mapi (attr_trait ctx)   c.attrs
	]
    }

  let make_traits ~base ~name ~data xs =
    List.mapi (fun id x -> {
		 AbcType.trait_name = name x;
		 data               = data (id+base);
		 trait_metadata     = []
	       })  xs

  let __to_cpool top_method =
    let ms =
      methods top_method in
    let cs =
      classes ms in
      consts ms cs

  let to_abc slots top_method =
    let ms =
      methods top_method in
    let cs =
      classes ms in
    let cpool =
      consts ms cs in
    let ctx =
      {| cpool = cpool; methods = ms; classes = cs |} in
    let classes    =
      List.map  (class_info ctx)    cs in
    let instances =
      List.map  (instance_info ctx) cs in
    let slot_traits =
      make_traits ~base:1
	          ~name:(Cpool.index cpool)
	          ~data:(fun id -> AbcType.SlotTrait (id,0,0,0))
	          slots in
    let n =
      List.length slots in
    let class_traits =
      make_traits ~base:0
	          ~name:(fun {AbcType.instance_name=name} -> name)
	          ~data:(fun id -> AbcType.ClassTrait (id+n+1,id))
	          instances in
      {
	AbcType.cpool = Cpool.to_abc cpool;
	metadata  = [];
	method_info   = List.map  (method_info cpool) ms;
	method_bodies = List.mapi (method_body ctx)  ms;
	classes;
	instances;
	scripts=[{ AbcType.init  = index top_method ms;
		   script_traits = slot_traits @ class_traits }]
      }
end
