open Base
open Swflib
open Swflib.AbcType

type reloc = int -> int

let reloc_ns relocs =
  function
      Namespace name ->
	Namespace (relocs#string name)
    | PackageNamespace name ->
	PackageNamespace (relocs#string name)
    | PackageInternalNamespace name ->
	PackageInternalNamespace (relocs#string name)
    | ProtectedNamespace name ->
	ProtectedNamespace (relocs#string name)
    | ExplicitNamespace name ->
	ExplicitNamespace (relocs#string name)
    | StaticProtectedNamespace name ->
	StaticProtectedNamespace (relocs#string name)
    | PrivateNamespace name ->
	PrivateNamespace (relocs#string name)

let reloc_nss relocs =
  List.map relocs#namespace

let reloc_multi relocs = function
    QName (ns, name) ->
      QName (relocs#namespace ns, relocs#string name)
  | QNameA (ns, name) ->
      QNameA (relocs#namespace ns, relocs#string name)
  | RTQName name ->
      RTQName (relocs#string name)
  | RTQNameA name ->
      RTQNameA (relocs#string name)
  | RTQNameL | RTQNameLA as n ->
      n
  | Multiname (name, nss) ->
      Multiname (relocs#string name, relocs#namespace_set nss)
  | MultinameA (name, nss) ->
      MultinameA (relocs#string name, relocs#namespace_set nss)
  | MultinameL name ->
      MultinameL (relocs#string name)
  | MultinameLA name ->
      MultinameLA (relocs#string name)

let rmap f relocs x =
  List.map (f relocs) x

let reloc_cpool relocs cpool =
  { cpool with
      namespace     = rmap reloc_ns    relocs cpool.namespace;
      namespace_set = rmap reloc_nss   relocs cpool.namespace_set;
      multiname     = rmap reloc_multi relocs cpool.multiname }

(* trait *)
let reloc_trait_data relocs = function
    SlotTrait (id,name,vindex,vkind) ->
      SlotTrait (id, relocs#multiname  name, vindex, vkind)
  | ConstTrait (id,name,vindex,vkind) ->
      ConstTrait (id, relocs#multiname name, vindex, vkind)
  | ClassTrait (id, classi) ->
      ClassTrait (id, relocs#classes classi)
  | MethodTrait (id, methodi,attrs) ->
      MethodTrait (id, relocs#methods methodi,attrs)
  | SetterTrait (id, methodi,attrs) ->
      SetterTrait (id, relocs#methods methodi, attrs)
  | GetterTrait (id, methodi,attrs) ->
      GetterTrait (id, relocs#methods methodi, attrs)
  | FunctionTrait (id, funi) ->
      FunctionTrait (id, relocs#methods funi)

let reloc_traits relocs =
  rmap begin fun relocs t -> {
    t with
      trait_name = relocs#multiname t.trait_name;
      data       = reloc_trait_data relocs t.data
  } end relocs

(* method *)
let reloc_code relocs : Swflib.LowInst.t -> Swflib.LowInst.t = function
    `PushString i ->
      `PushString (relocs#string i)
  | `PushInt i ->
      `PushInt (relocs#int i)
  | `PushUInt i ->
      `PushUInt (relocs#uint i)
  | `PushDouble i ->
      `PushDouble (relocs#double i)
  | `GetLex i ->
      `GetLex (relocs#multiname i)
  | `GetProperty i ->
      `GetProperty (relocs#multiname i)
  | `SetProperty  i ->
      `SetProperty (relocs#multiname i)
  | `InitProperty i ->
      `InitProperty (relocs#multiname i)
  | `FindPropStrict i ->
      `FindPropStrict (relocs#multiname i)
  | `CallProperty (i,count) ->
      `CallProperty (relocs#multiname i, count)
  | `CallPropLex (i,count) ->
      `CallPropLex (relocs#multiname i, count)
  | `ConstructProp (i,count) ->
      `ConstructProp (relocs#multiname i, count)
  | `NewClass i ->
      `NewClass (relocs#classes i)
  | `NewFunction i ->
      `NewFunction (relocs#methods i)
  | _ as i ->
      i

let reloc_method_info relocs m =
  { m with
      method_name = relocs#multiname m.method_name }

let reloc_method relocs m =
  { m with
      code          = rmap reloc_code relocs m.code;
      method_traits = reloc_traits relocs m.method_traits
  }

(* class *)
let reloc_class relocs c =
  {
    cinit        = relocs#methods c.cinit;
    class_traits = reloc_traits relocs c.class_traits
  }

let reloc_instance relocs i =
  {i with
     instance_name   = relocs#multiname i.instance_name;
     super_name      = relocs#multiname i.super_name;
     iinit           = relocs#methods i.iinit;
     instance_traits = reloc_traits relocs i.instance_traits }

(* script *)
let reloc_script relocs s =
  {
    init = relocs#methods s.init;
    script_traits = reloc_traits relocs s.script_traits
  }

let reloc relocs abc =
  { abc with
      cpool         = reloc_cpool relocs abc.cpool;
      method_info   = rmap reloc_method_info relocs abc.method_info;
      method_bodies = rmap reloc_method      relocs abc.method_bodies;
      classes       = rmap reloc_class       relocs abc.AbcType.classes;
      instances     = rmap reloc_instance    relocs abc.instances;
      scripts       = rmap reloc_script      relocs abc.scripts
  }

