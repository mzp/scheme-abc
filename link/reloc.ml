open Base
open Swflib
open Swflib.AbcType

type reloc = int -> int
type t = {
  int       : reloc;
  uint      : reloc;
  double    : reloc;
  string    : reloc;
  namespace     : reloc;
  namespace_set : reloc;
  multiname : reloc;
  methods   : reloc;
  classes   : reloc
}

let reloc_ns {string} =
  function
      Namespace name ->
	Namespace (string name)
    | PackageNamespace name ->
	PackageNamespace (string name)
    | PackageInternalNamespace name ->
	PackageInternalNamespace (string name)
    | ProtectedNamespace name ->
	ProtectedNamespace (string name)
    | ExplicitNamespace name ->
	ExplicitNamespace (string name)
    | StaticProtectedNamespace name ->
	StaticProtectedNamespace (string name)
    | PrivateNamespace name ->
	PrivateNamespace (string name)

let reloc_nss { namespace } =
  List.map namespace

let reloc_multi {namespace; namespace_set; string} = function
    QName (ns, name) ->
      QName (namespace ns, string name)
  | QNameA (ns, name) ->
      QNameA (namespace ns, string name)
  | RTQName name ->
      RTQName (string name)
  | RTQNameA name ->
      RTQNameA (string name)
  | RTQNameL | RTQNameLA as n ->
      n
  | Multiname (name, nss) ->
      Multiname (string name, namespace_set nss)
  | MultinameA (name, nss) ->
      MultinameA (string name, namespace_set nss)
  | MultinameL name ->
      MultinameL (string name)
  | MultinameLA name ->
      MultinameLA (string name)

let rmap f ctx x =
  List.map (f ctx) x

let reloc_cpool ctx cpool =
  { cpool with
      AbcType.namespace = rmap reloc_ns    ctx cpool.AbcType.namespace;
      namespace_set     = rmap reloc_nss   ctx cpool.AbcType.namespace_set;
      multiname         = rmap reloc_multi ctx cpool.AbcType.multiname }

(* trait *)
let reloc_trait_data {multiname; classes; methods} = function
    SlotTrait (id,name,vindex,vkind) ->
      SlotTrait (id, multiname  name, vindex, vkind)
  | ConstTrait (id,name,vindex,vkind) ->
      ConstTrait (id, multiname name, vindex, vkind)
  | ClassTrait (id, classi) ->
      ClassTrait (id, classes classi)
  | MethodTrait (id, methodi,attrs) ->
      MethodTrait (id, methods methodi,attrs)
  | SetterTrait (id, methodi,attrs) ->
      SetterTrait (id, methods methodi, attrs)
  | GetterTrait (id, methodi,attrs) ->
      GetterTrait (id, methods methodi, attrs)
  | FunctionTrait (id, funi) ->
      FunctionTrait (id, methods funi)

let reloc_traits ctx =
  rmap begin fun ctx t -> {
    t with
      trait_name = ctx.multiname t.trait_name;
      data       = reloc_trait_data ctx t.data
  } end ctx

(* method *)
let reloc_code ctx : Swflib.LowInst.t -> Swflib.LowInst.t = function
    `PushString i ->
      `PushString (ctx.string i)
  | `PushInt i ->
      `PushInt (ctx.int i)
  | `PushUInt i ->
      `PushUInt (ctx.uint i)
  | `PushDouble i ->
      `PushDouble (ctx.double i)
  | `GetLex i ->
      `GetLex (ctx.multiname i)
  | `GetProperty i ->
      `GetProperty (ctx.multiname i)
  | `SetProperty  i ->
      `SetProperty (ctx.multiname i)
  | `InitProperty i ->
      `InitProperty (ctx.multiname i)
  | `FindPropStrict i ->
      `FindPropStrict (ctx.multiname i)
  | `CallProperty (i,count) ->
      `CallProperty (ctx.multiname i, count)
  | `CallPropLex (i,count) ->
      `CallPropLex (ctx.multiname i, count)
  | `ConstructProp (i,count) ->
      `ConstructProp (ctx.multiname i, count)
  | `NewClass i ->
      `NewClass (ctx.classes i)
  | `NewFunction i ->
      `NewFunction (ctx.methods i)
  | _ as i ->
      i

let reloc_method_info {multiname} m =
  { m with
      method_name = multiname m.method_name }

let reloc_method ctx m =
  { m with
      method_sig = ctx.methods m.method_sig;
      code       = rmap reloc_code ctx m.code;
      method_traits = reloc_traits ctx m.method_traits
  }

(* class *)
let reloc_class ctx c =
  {
      cinit = ctx.methods c.cinit;
      class_traits = reloc_traits ctx c.class_traits
  }

let reloc_instance ctx i =
  {i with
     instance_name   = ctx.multiname i.instance_name;
     super_name      = ctx.multiname i.super_name;
     iinit           = ctx.methods i.iinit;
     instance_traits = reloc_traits ctx i.instance_traits }

(* script *)
let reloc_script ctx s =
  {
    init = ctx.methods s.init;
    script_traits = reloc_traits ctx s.script_traits
  }

let reloc ctx abc =
  { abc with
      cpool           = reloc_cpool ctx abc.cpool;
      method_info     = rmap reloc_method_info ctx abc.method_info;
      method_bodies   = rmap reloc_method      ctx abc.method_bodies;
      AbcType.classes = rmap reloc_class       ctx abc.AbcType.classes;
      instances       = rmap reloc_instance    ctx abc.instances;
      scripts         = rmap reloc_script      ctx abc.scripts
  }

