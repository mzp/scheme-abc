open Base
open EasyXml
open Swflib.AbcType

let some x =
  match x with Some _ -> "1" | None -> "0"

let bool x =
  if x then "1" else "0"

let u30 x =
  attr "U30" ["value",string_of_int x]

let value name x =
  attr name ["value",x]

let index_attr name xs =
  attr name @@ List.map (fun (x,y) -> (x,string_of_int y)) xs

let elem_with name f xs =
  elem name @@ List.map f xs

let of_namespace ns =
  let make name index =
    element name ["index", string_of_int index] [] in
    match ns with
	Namespace name ->
	  make "Namespace" name
      | PackageNamespace name ->
	  make "PackageNamespace" name
      |	PackageInternalNamespace name ->
	  make "PackageInternalNamespace" name
      | ProtectedNamespace name ->
	  make "ProtectedNamespace" name
      | ExplicitNamespace name ->
	  make "ExplicitNamespace" name
      | StaticProtectedNamespace _ ->
	  todo "this namespace is not support."
      | PrivateNamespace name ->
	  make "PrivateNamespace" name

let of_ns_set ns_set =
  elem "namespaces" @@
    List.map u30 ns_set

let of_multiname =
  function
      QName (ns,name) ->
	index_attr "QName" ["nameIndex",name; "namespaceIndex",ns]
    | QNameA (ns,name) ->
	index_attr "QNameA" ["namespaceIndex",ns; "nameIndex",name]
    | RTQName (name) ->
	index_attr "RTQName" ["nameIndex",name]
    | RTQNameA (name) ->
	index_attr "RTQNameA" ["nameIndex",name]
    | RTQNameL ->
	index_attr "RTQNameL" []
    | RTQNameLA ->
	index_attr "RTQNameLA" []
    | Multiname (name,ns_set) ->
	index_attr "Multiname" ["nameIndex",name; "namespaceSetIndex",ns_set]
    | MultinameA (name,ns_set) ->
	index_attr "MultinameA" ["nameIndex",name; "namespaceSetIndex",ns_set]
    | MultinameL ns_set ->
	index_attr "MultinameL" ["namespaceSetIndex",ns_set]
    | MultinameLA ns_set ->
	index_attr "MultinameLA" ["namespaceSetIndex",ns_set]

let of_cpool cpool =
  elem "Cpool"
    [
      elem "ints"    @@ List.map u30 cpool.int;
      elem "uints"   @@ List.map u30 cpool.uint;
      elem "doubles" @@ List.map (value "Double" $ string_of_float) cpool.double;
      elem "strings" @@ List.map (value "String2") cpool.string;
      elem "namespaces" @@ List.map of_namespace cpool.namespace;
      elem "namespaceSets" @@ List.map of_ns_set cpool.namespace_set;
      elem "multinames" @@ List.map of_multiname cpool.multiname
    ]

let of_method_flags xs =
  List.map begin function
      NeedArguments ->
	("needArguments" ,bool true)
    | NeedActivation ->
	("needActivation",bool true)
    | NeedRest ->
	("needRest"      ,bool true)
    | SetDxns ->
	("setSDXNs"      ,bool true)
    | HasOptional _ ->
	("hasOptional"   ,bool true)
    | HasParamNames _ ->
	("hasParamNames" ,bool true)
  end xs

let of_methods xs =
    elem_with "methods"
      (fun m ->
	 element "MethodInfo" (["retType"       ,string_of_int m.return;
			       "nameIndex"     ,string_of_int m.method_name;
			       "isExplicit"    ,"0"] @
				 of_method_flags m.method_flags)
	   [elem "paramTypes" @@ List.map u30 m.params])
      xs

let of_metadata xs =
  elem_with "metadata"
    (fun m ->
       element "MetadataInfo" ["nameIndex",string_of_int m.metadata_name]
	 [elem "keys"   @@ List.map (u30 $ fst) m.items;
	  elem "values" @@ List.map (u30 $ snd) m.items ]) xs

let of_trait {trait_name=name; data=data; trait_metadata=metadata } =
  let trait =
    match data with
        ClassTrait (slotID,classi) ->
	  index_attr "Class" ["slotID",slotID;
			      "classInfo",classi]
      | SlotTrait (id,name,vindex,kind) ->
          index_attr "Slot" @@ (
	    ["slotID"    ,id;
	     "typeIndex" ,name;
	     "valueIndex",vindex;
	    ] @ match vindex with
		0 -> [ ]
	      | _ -> ["valueKind",kind])
      | ConstTrait (id,name,vindex,kind) ->
          index_attr "Slot" @@ (
	    [
	      "slotID"    ,id;
	      "typeIndex" ,name;
	      "valueIndex",vindex;
	    ] @ match vindex with
		0 -> [ ]
	      | _ -> ["valueKind", kind])
      | FunctionTrait (id,funi) ->
	  index_attr "Function"
	    ["slotID",id;
	     "methodInfo",funi]
      | GetterTrait (id,methodi, _) ->
	  index_attr "Getter" ["dispID",id;
			       "methodInfo",methodi]
      | MethodTrait (id,methodi, _) ->
	  index_attr "Method" ["dispID",id;
			       "methodInfo",methodi]
      | SetterTrait (id,methodi, _) ->
	  index_attr "Setter" ["dispID",id;
			       "methodInfo",methodi] in
  let attrs =
    match data with
      | GetterTrait (_, _, attrs) | MethodTrait (_, _, attrs) | SetterTrait (_, _, attrs) ->
	  attrs
      | SlotTrait _ | ConstTrait _  | FunctionTrait _ | ClassTrait _ ->
	  []
  in
    element "TraitInfo" [ "nameIndex"  ,string_of_int name;
			  "hasMetadata",bool (metadata<>[]);
			  "override"   ,bool (List.mem ATTR_Override attrs);
			  "final"      ,bool (List.mem ATTR_Final attrs)] @@
      [elem "trait" [trait]]

let of_instance_flags xs =
  HList.concat_map begin function
      Interface ->
	["interface", bool true]
    | Final ->
	["final", bool true]
    | Sealed ->
	["sealed",bool true]
    | ProtectedNs ns ->
	["isProtectedNs",bool true;
	 "protectedNS"  ,string_of_int ns]
  end xs

let of_instances instances =
  elem_with "instances"
    (fun i ->
       element "InstanceInfo"
	 (["nameIndex"     ,string_of_int i.instance_name;
	   "superIndex"    ,string_of_int i.super_name;
	   "iInitIndex"    ,string_of_int i.iinit]
	  @ of_instance_flags i.instance_flags)
         [elem "interfaces" @@ List.map u30 i.interfaces;
	  elem "traits" @@ List.map of_trait i.instance_traits])
    instances

let of_classes xs =
  elem_with "classes"
    (fun c ->
       element "ClassInfo"
	 ["cInitIndex",string_of_int c.cinit]
	 [elem "traits" @@ List.map of_trait c.class_traits]) xs

let of_exception e =
  index_attr "Exception" [
    "tryStart",e.from_pos;
    "tryEnd"  ,e.to_pos;
    "target"  ,e.target;
    "type"    ,e.exception_type;
    "name"    ,e.var_name ]

let of_method_bodies xs =
  elem_with "methodBodies"
    (fun m ->
       element "MethodBody" ["exceptionCount",string_of_int @@ List.length m.exceptions;
			     "maxRegs"       ,string_of_int m.local_count;
			     "maxScope"      ,string_of_int m.max_scope_depth;
			     "maxStack"      ,string_of_int m.max_stack;
			     "methodInfo"    ,string_of_int m.method_sig;
			     "scopeDepth"    ,string_of_int m.init_scope_depth]
	 [elem_with "code"       Code.to_xml      m.code;
	  elem_with "exceptions" of_exception m.exceptions;
	  elem_with "traits"     of_trait     m.method_traits ]) xs


let of_script scripts =
  elem_with "scripts"
    (fun s ->
       element "ScriptInfo" ["initIndex",string_of_int s.init] @@
	 [elem_with "traits" of_trait s.script_traits] )
    scripts


let of_abc abc =
  element "Action3"
    ["minorVersion",string_of_int Swflib.AbcIn.cMinorVersion;
     "majorVersion",string_of_int Swflib.AbcIn.cMajorVersion]
    [elem "constants" [of_cpool abc.cpool];
     of_methods   abc.method_info;
     of_metadata  abc.metadata;
     of_instances abc.instances;
     of_classes   abc.classes;
     of_script    abc.scripts;
     of_method_bodies abc.method_bodies]

let to_xml =
  of_abc


