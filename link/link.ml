open Base
open Swflib.AbcType

let link_cpool c1 c2 = {
  int = c1.int @ c2.int;
  uint = c1.uint @ c2.uint;
  double = c1.double @ c2.double;
  string = c1.string @ c2.string;
  namespace = c1.namespace @ c2.namespace;
  namespace_set = c1.namespace_set @ c2.namespace_set;
  multiname = c1.multiname @ c2.multiname
}

let link _ _ = undef
