open Base
open Swflib.AbcType

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

let reloc ctx f xs =
  List.map (f ctx) xs

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
      namespace     = c1.namespace     @ reloc ctx reloc_ns    c2.namespace;
      namespace_set = c1.namespace_set @ reloc ctx reloc_nss   c2.namespace_set;
      multiname     = c1.multiname     @ reloc ctx reloc_multi c2.multiname;
    }

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

let reloc_method ctx m =
  { m with
      method_sig = m.method_sig + ctx#info;
      code       = reloc ctx reloc_code m.code }

let link a1 a2 =
  let ctx = {|
      cpool = {|
	  int    = List.length a1.cpool.int;
	  uint   = List.length a1.cpool.uint;
	  double = List.length a1.cpool.double;
	  string = List.length a1.cpool.string
      |};
      info = List.length a1.method_info
  |} in
    { a1 with
	cpool         = link_cpool a1.cpool a2.cpool;
	method_info   = a1.method_info   @ a2.method_info;
	method_bodies = a1.method_bodies @ reloc ctx reloc_method a2.method_bodies}
