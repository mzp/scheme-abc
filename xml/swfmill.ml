open Base
open EasyXml

let some x =
  match x with Some _ -> "1" | None -> "0"

let bool x =
  if x then "1" else "0"

let u30 x =
  attr "U30" ["value",Int32.to_string x]

let value name x =
  attr name ["value",x]

let index_attr name xs =
  attr name @@ List.map (fun (x,y) -> (x,Int32.to_string y)) xs

let elem_with name f xs =
  elem name @@ List.map f xs

let of_namespace ns =
  let make name index =
    element name ["index",Int32.to_string index] [] in
    match ns with
	`Namespace name ->
	  make "Namespace" name
      | `PackageNamespace name ->
	  make "PackageNamespace" name
      |	`PackageInternaNs name ->
	  make "PackageInternalNamespace" name
      | `ProtectedNamespace name ->
	  make "ProtectedNamespace" name
      | `ExplicitNamespace name ->
	  make "ExplicitNamespace" name
      | `StaticProtectedNs name ->
	  todo "this namespace is not support."
      | `PrivateNs name ->
	  make "PrivateNamespace" name

let of_ns_set ns_set =
  elem "namespaces" @@
    List.map (value "U30" $ Int32.to_string) ns_set#ns

let of_multiname =
  function
      `QName o ->
	index_attr "QName" ["nameIndex",o#name; "namespaceIndex",o#ns]
    | `QNameA o ->
	index_attr "QNameA" ["namespaceIndex",o#ns; "nameIndex",o#name]
    | `RTQName o ->
	index_attr "RTQName" ["nameIndex",o#name]
    | `RTQNameA o ->
	index_attr "RTQNameA" ["nameIndex",o#name]
    | `RTQNameL ->
	index_attr "RTQNameL" []
    | `RTQNameLA ->
	index_attr "RTQNameLA" []
    | `Multiname o ->
	index_attr "Multiname" ["nameIndex",o#name; "namespaceSetIndex",o#ns_set]
    | `MultinameA o ->
	index_attr "MultinameA" ["nameIndex",o#name; "namespaceSetIndex",o#ns_set]
    | `MultinameL o ->
	index_attr "MultinameL" ["namespaceSetIndex",o#ns_set]
    | `MultinameLA o ->
	index_attr "MultinameLA" ["namespaceSetIndex",o#ns_set]

let of_cpool constants =
  elem "Constants"
    [
      elem "ints"    @@ List.map (value "U30" $ Int32.to_string) constants#integer;
      elem "uints"   @@ List.map (value "U30" $ Int32.to_string) constants#uinteger;
      elem "doubles" @@ List.map (value "Double" $ string_of_float) constants#double;
      elem "strings" @@ List.map (value "String2") constants#string;
      elem "namespaces" @@ List.map of_namespace constants#namespace;
      elem "namespaceSets" @@ List.map of_ns_set constants#ns_set;
      elem "multinames" @@ List.map of_multiname constants#multiname
    ]

let of_methods xs =
    elem_with "methods"
      (fun m -> 
	 element "MethodInfo" ["retType"       ,Int32.to_string m#return_type;
			       "nameIndex"     ,Int32.to_string m#name;
			       "hasParamNames" ,some m#param_names;
			       "setSDXNs"      ,bool m#set_dxns;
			       "isExplicit"    ,"0";
			       "ignoreRest"    ,bool m#need_rest;
			       "hasOptional"   ,some m#options;
			       "needRest"      ,bool m#need_rest;
			       "needActivation",bool m#need_activation;
			       "needArguments" ,bool m#need_arguments]
	   [elem "paramTypes" @@ List.map u30 m#param_types]) xs

let of_metadata xs =
  elem_with "metadata"
    (fun m ->
       element "MetadataInfo" ["nameIndex",Int32.to_string m#name]
	 [elem "keys"   @@ List.map (fun i-> u30 i#key)   m#items;
	  elem "values" @@ List.map (fun i-> u30 i#value) m#items ]) xs

let of_trait trait =
  element "TraitInfo" [ "nameIndex"  ,Int32.to_string trait#name;
			"hasMetadata",some trait#metadata;
			"override"   ,bool trait#attr_override;
			"final"      ,bool trait#attr_final]
    [ elem "trait" [
	match trait#data with
            `Class t ->
	      attr "Class" ["slotID",Int32.to_string t#slot_id;
			    "classInfo",Int32.to_string t#classi]
          | `Slot t ->
              index_attr "Slot" @@ (["slotID"    ,t#slot_id;
				     "typeIndex" ,t#type_name;
				     "valueIndex",t#vindex;
	      ] @ match t#vkind with
		  None -> [ ]
		| Some kind -> ["valueKind",Int32.of_int kind])
          | `Const t ->
              attr "Slot" @@ ([
		"slotID"    ,Int32.to_string t#slot_id;
		"typeIndex" ,Int32.to_string t#type_name;
		"valueIndex",Int32.to_string t#vindex;
	      ] @ match t#vkind with
		  None -> [ ]
		| Some kind -> ["valueKind",string_of_int kind])
          | `Function t ->
	      attr "Function" ["slotID",Int32.to_string t#slot_id;
			       "methodInfo",Int32.to_string  t#functioni]
          | `Getter t ->
	      attr "Getter" ["dispID",Int32.to_string t#disp_id;
			     "methodInfo",Int32.to_string t#methodi]
          | `Method t ->
	      attr "Method" ["dispID",Int32.to_string t#disp_id;"methodInfo",Int32.to_string t#methodi]
          | `Setter t ->
	      attr "Setter" ["dispID",Int32.to_string t#disp_id;"methodInfo",Int32.to_string t#methodi]
      ]]


let of_instances instances =
  elem_with "instances"
    (fun i -> 
       element "InstanceInfo" 
	 (["nameIndex"    ,Int32.to_string i#name;
	   "superIndex"    ,Int32.to_string i#super_name;
	   "hasProtectedNS",bool i#is_protected;
	   "interface"     ,bool i#is_interface;
	   "final"         ,bool i#is_final;
	   "sealed"        ,bool i#is_sealed;
	   "iInitIndex"    ,Int32.to_string i#iinit] @ 
	    (match i#protectedNs with
		 None -> []
	       | Some x ->
		   ["protectedNS",Int32.to_string x])) @@
         [elem "interfaces" @@ List.map u30 i#interface;
	  elem "traits" @@ List.map of_trait i#traits]) instances

let of_classes xs =
  elem_with "classes" 
    (fun c ->
       element "ClassInfo" 
	 ["cInitIndex",Int32.to_string c#cinit] 
	 [elem "traits" @@ List.map of_trait c#traits]) xs

let of_exception e = 
  index_attr "Exception" [
    "tryStart",e#from_pos;
    "tryEnd"  ,e#to_pos;
    "target"  ,e#target;
    "type"    ,e#exc_type;
    "name"    ,e#var_name ]

let of_method_bodies xs = 
  elem_with "methodBodies" 
    (fun m ->
       element "MethodBody" ["exceptionCount",string_of_int @@ List.length m#exceptions;
			     "maxRegs"       ,Int32.to_string m#local_count;
			     "maxScope"      ,Int32.to_string m#max_scope_depth;
			     "maxStack"      ,Int32.to_string m#max_stack;
			     "methodInfo"    ,Int32.to_string m#methodi;
			     "scopeDepth"    ,Int32.to_string m#init_scope_depth]
	 [elem_with "code"       Code.to_xml      m#code;
	  elem_with "exceptions" of_exception m#exceptions;
	  elem_with "traits"     of_trait     m#traits ]) xs
    

let of_script scripts =
  elem_with "scripts"
    (fun s ->
       element "ScriptInfo" ["initIndex",Int32.to_string s#init] @@
	 [elem_with "traits" of_trait s#traits] )
    scripts
  

let of_abc abc =
  element "Action3" 
    ["minorVersion",string_of_int abc#minor_version;
     "majorVersion",string_of_int abc#major_version]
    [elem "constants" [of_cpool abc#constant_pool];
     of_methods   abc#methods;
     of_metadata  abc#metadata;
     of_instances abc#instances;
     of_classes   abc#classes;
     of_script    abc#script;
     of_method_bodies abc#method_body]

let to_xml =
  of_abc

let f () =
  let ch = 
    open_in_bin "a.abc" in
    Abc.of_stream @@ Byte.of_channel ch

let sample () =
  let ch = 
    open_in_bin "a.abc" in
    to_xml @@ Abc.of_stream @@ Byte.of_channel ch
