open Base
let element name child =
  Xml.Element (name,[],child)

let attr name attr value =
  Xml.Element (name,[attr,value],[])

let u30 =
  attr "U30" "value"

let i2s =
  string_of_int

let of_multiname =
  function 
      Abc.QName (ns,name) ->
	Xml.Element ("QName",
		     ["namespaceIndex",i2s ns;
		      "nameIndex"     ,i2s name],[])
    | Abc.Multiname (name,nss) ->
	Xml.Element ("Multiname",
		     ["nameIndex",i2s name;
		      "namespaceSetIndex",i2s nss],[])
let of_cpool {
  Abc.int       = ints;
  uint          = uints;
  double        = doubles;
  string        = strings;
  namespace     = ns;
  namespace_set = nss;
  multiname     = mn;
} =
  element "Constants"
    [element "ints"          @@ List.map (u30 $ i2s) ints;
     element "uints"         @@ List.map (u30 $ i2s) uints;
     element "doubles"       @@ List.map (u30 $ string_of_float) doubles;
     element "strings"       @@ List.map (attr "String2" "value") strings;
     element "namespaces"    @@ 
       List.map (fun {Abc.ns_name=i}-> attr "Namespace" "index" @@ i2s i) ns;
     element "namespaceSets" @@ 
       List.map (element "NamespaceSet" $ List.map (u30 $ i2s)) nss;
     element "multinames"    @@
       List.map of_multiname mn;
    ]

let of_method_info {
  Abc.params = params;
  return     = return;
  name       = name;
  flags      = flags;
} =
  Xml.Element ("MethodInfo",
	       ["retType"       ,i2s return;
		"nameIndex"     ,i2s name;
		"hasParamNames" ,"0";
		"setSDXNs"      ,"0";
		"isExplicit"    ,"0";
		"ignoreRest"    ,"0";
		"hasOptional"   ,"0";
		"needRest"      ,"0";
		"needActivation","0";
		"needArguments" ,"0";],
	       [element "paramTypes" @@ List.map (u30 $ i2s) params])

(*
val of_script : Abc.script -> Bytes.t list
val of_trait : Abc.trait -> Bytes.t list
val of_method_body : Abc.method_body -> Bytes.t list

val of_class : Abc.class_info -> Bytes.t list
val of_instance : Abc.instance_info -> Bytes.t list
*)
