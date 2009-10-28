open Base
open Swflib.AbcType

let reloc ctx f xs =
  List.map (f ctx) xs

let link f ctx x y =
  x @ reloc ctx f y

(* cpool *)
let reloc_ns ctx ns =
  let n =
    ctx#str in
    match ns with
	Namespace name ->
	  Namespace (name + n)
      | PackageNamespace name ->
	  PackageNamespace (name + n)
      | PackageInternalNamespace name ->
	  PackageInternalNamespace (name + n)
      | ProtectedNamespace name ->
	  ProtectedNamespace (name + n)
      | ExplicitNamespace name ->
	  ExplicitNamespace (name + n)
      | StaticProtectedNamespace name ->
	  StaticProtectedNamespace (name + n)
      | PrivateNamespace name ->
	  PrivateNamespace (name + n)

let reloc_nss ctx =
  List.map (fun ns-> ns + ctx#ns)

let reloc_multi ctx = function
    QName (ns, name) ->
      QName (ns + ctx#ns, name + ctx#str)
  | QNameA (ns, name) ->
      QNameA (ns + ctx#ns, name + ctx#str)
  | RTQName name ->
      RTQName (name + ctx#str)
  | RTQNameA name ->
      RTQNameA (name + ctx#str)
  | RTQNameL | RTQNameLA as n ->
      n
  | Multiname (name, nss) ->
      Multiname (name + ctx#str, nss + ctx#nss)
  | MultinameA (name, nss) ->
      MultinameA (name + ctx#str, nss + ctx#nss)
  | MultinameL name ->
      MultinameL (name + ctx#str)
  | MultinameLA name ->
      MultinameLA (name + ctx#str)

let link_cpool c1 c2 =
  let ctx = {|
      str = List.length c1.string;
      ns  = List.length c1.namespace;
      nss =List.length c1.namespace_set
  |} in
    {
      int           = c1.int           @ c2.int;
      uint          = c1.uint          @ c2.uint;
      double        = c1.double        @ c2.double;
      string        = c1.string        @ c2.string;
      namespace     = link reloc_ns    ctx c1.namespace     c2.namespace;
      namespace_set = link reloc_nss   ctx c1.namespace_set c2.namespace_set;
      multiname     = link reloc_multi ctx c1.multiname     c2.multiname;
    }

let reloc_name ctx name =
  if name = 0 then
    0
  else
    name + ctx#cpool#multiname

(* trait *)
let reloc_trait_data ctx = function
    SlotTrait (id,name,vindex,vkind) ->
      SlotTrait (id, reloc_name ctx name, vindex, vkind)
  | ConstTrait (id,name,vindex,vkind) ->
      ConstTrait (id, reloc_name ctx name, vindex, vkind)
  | ClassTrait (id, classi) ->
      ClassTrait (id, classi + ctx#classes)
  | MethodTrait (id, methodi,attrs) ->
      MethodTrait (id, methodi + ctx#methods,attrs)
  | SetterTrait (id, methodi,attrs) ->
      SetterTrait (id, methodi + ctx#methods,attrs)
  | GetterTrait (id, methodi,attrs) ->
      GetterTrait (id, methodi + ctx#methods,attrs)
  | FunctionTrait (id, funi) ->
      FunctionTrait (id, funi+ctx#methods)

let reloc_traits ctx =
  reloc ctx begin fun ctx t ->
    {t with
       trait_name = reloc_name ctx t.trait_name;
       data = reloc_trait_data ctx t.data
    }
  end


(* method *)
let reloc_code ctx : Swflib.LowInst.t -> Swflib.LowInst.t = function
    `PushString i ->
      `PushString (i + ctx#cpool#string)
  | `PushInt i ->
      `PushInt (i + ctx#cpool#int)
  | `PushUInt i ->
      `PushUInt (i + ctx#cpool#uint)
  | `PushDouble i ->
      `PushDouble (i + ctx#cpool#double)
  | _ as i ->
      i

let reloc_method_info ctx m =
  { m with
      method_name = reloc_name ctx m.method_name }

let reloc_method ctx m =
  { m with
      method_sig = m.method_sig + ctx#methods;
      code       = reloc ctx reloc_code m.code }

(* class *)
let reloc_class ctx c =
  {
      cinit = c.cinit + ctx#methods;
      class_traits = reloc_traits ctx c.class_traits
  }

let reloc_instance ctx i =
  {i with
     instance_name = reloc_name ctx i.instance_name;
     super_name    = reloc_name ctx i.super_name;
     iinit = i.iinit + ctx#methods }

let link a1 a2 =
  let ctx = {|
      cpool = {|
	  int    = List.length a1.cpool.int;
	  uint   = List.length a1.cpool.uint;
	  double = List.length a1.cpool.double;
	  string = List.length a1.cpool.string;
	  multiname =  List.length a1.cpool.multiname
      |};
      methods = List.length a1.method_info;
      classes = List.length a1.classes
  |} in
    { a1 with
	cpool         = link_cpool a1.cpool a2.cpool;
	method_info   = link reloc_method_info ctx a1.method_info   a2.method_info;
	method_bodies = link reloc_method      ctx a1.method_bodies a2.method_bodies;
	classes       = link reloc_class       ctx a1.classes       a2.classes;
	instances     = link reloc_instance    ctx a1.instances     a2.instances
    }
