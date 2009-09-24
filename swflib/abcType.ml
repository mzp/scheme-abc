open Base

type namespace = {
  kind:int; namespace_name:int
}

type namespace_set = int list

type multiname =
    QName     of int * int
  | Multiname of int * int

type cpool = {
  int: int list;
  uint: int list;
  double: float list;
  string: string list;
  namespace: namespace list;
  namespace_set: namespace_set list;
  multiname: multiname list;
}

type method_info = {
  params: int list;
  return: int;
  method_name: int;
  method_flags: int;
}

(* TODO *)
type trait_attr =
    ATTR_Final | ATTR_Override | ATTR_Medadata

type trait_data =
    SlotTrait   of int * int * int * int
  | MethodTrait of int * int * trait_attr list
  | GetterTrait of int * int * trait_attr list
  | SetterTrait of int * int * trait_attr list
  | ClassTrait  of int * int
  | FunctionTrait of int * int
  | ConstTrait    of int * int * int * int

type trait = {
  trait_name:int;
  data:trait_data
}

type script = {
  init: int;
  script_traits: trait list
}

type class_info = {
  cinit: int;
  class_traits: trait list
}

type class_flag =
    Sealed | Final | Interface | ProtectedNs of int

type instance_info={
  instance_name:  int;
  super_name:     int;
  instance_flags: class_flag list;
  interface:      int list;
  iinit:          int;
  instance_traits:trait list
}

type exception_info = {
  from_pos : int;
  to_pos   : int;
  target   : int;
  exception_type : int;
  var_name : int
}

type 'a method_body = {
  method_sig:       int;
  max_stack:        int;
  local_count:      int;
  init_scope_depth: int;
  max_scope_depth:  int;
  code:             'a list;
  exceptions:       exception_info list;
  method_traits:    trait list
}

type metadata = {
  metadata_name : int;
  items: (int*int) list
}

type 'a t = {
  cpool:         cpool;
  method_info:   method_info list;
  metadata:      metadata list;
  classes:       class_info list;
  instances:     instance_info list;
  scripts:       script list;
  method_bodies: 'a method_body list
}
