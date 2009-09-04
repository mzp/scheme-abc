(**
    ABC(Action Script Bytecode) format.

    Provide the type of ABC and encoding function.

    @author mzp
    @see <http://www.adobe.com/devnet/actionscript/articles/avm2overview.pdf> AVM2 Overview(pdf) 4.2 abcFile - 4.10 Script
*)

type namespace = {
  kind:int; namespace_name:int
}

type namespace_set = int list

type multiname =
    QName     of int * int
  | Multiname of int * int

type cpool = {
  int:           int list;
  uint:          int list;
  double:        float list;
  string:        string list;
  namespace:     namespace list;
  namespace_set: namespace_set list;
  multiname:     multiname list;
}

type method_info = {
  params:      int list;
  return:      int;
  method_name: int;
  method_flags:int;
}

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

type method_body = {
  method_sig:       int;
  max_stack:        int;
  local_count:      int;
  init_scope_depth: int;
  max_scope_depth:  int;
  code:             Bytes.t list;
  exceptions:       int list;
  method_traits:    trait list
}

type abc = {
  cpool:       cpool;
  method_info: method_info list;
  metadata:    int list;
  classes:     class_info list;
  instances:   instance_info list;
  scripts:      script list;
  method_bodies: method_body list
}

(* cpool *)
val empty_cpool : cpool

(**
   Byte serializer for {!Abc}.
*)
val to_bytes : abc -> Bytes.t list

(**{6 Debug only}*)

val of_cpool : cpool -> Bytes.t list
val of_method_info : method_info -> Bytes.t list
val of_script : script -> Bytes.t list
val of_trait : trait -> Bytes.t list
val of_method_body : method_body -> Bytes.t list

val of_class : class_info -> Bytes.t list
val of_instance : instance_info -> Bytes.t list