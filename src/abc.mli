(** 
    ABC(Action Script Bytecode) format.

    Provide the type of ABC and encoding function.

    @author mzp
    @see <http://www.adobe.com/devnet/actionscript/articles/avm2overview.pdf> AVM2 Overview(pdf) 4.2 abcFile - 4.10 Script
*)

type trait_data =
    SlotTrait of int * int * int * int
  | MethodTrait of int * int
  | GetterTrait of int * int
  | SetterTrait of int * int
  | ClassTrait  of int * int
  | FunctionTrait of int * int
  | ConstTrait of int * int * int * int

type trait = {t_name:int; data:trait_data}

type script = {
  init: int;
  trait_s: trait list
}

type method_info = {
  params: int list;
  return: int;
  name:   int;
  flags:  int;
}

type method_body = {
  method_sig: int;
  max_stack: int;
  local_count: int;
  init_scope_depth: int;
  max_scope_depth: int;
  code: Bytes.t list;
  exceptions: int list;
  trait_m: trait list
}

type namespace = {
  kind:int; ns_name:int
}

type multiname = 
    QName of int*int 
  | Multiname of int*int

type namespace_set = int list
type cpool = {
  int:    int list;
  uint:  int list;
  double: float list;
  string: string list;
  namespace: namespace list;
  namespace_set:    namespace_set list;
  multiname: multiname list;
}

type class_info = {
  cinit: int;
  trait_c: trait list
}

(** AVM2 Overview: 4.7 Instance *)
type class_flag = Sealed | Final | Interface | ProtectedNs of int
type instance_info={
  name_i:      int;
  super_name:  int;
  flags_i:     class_flag list;
  interface:   int list;
  iinit:       int;
  trait_i:     trait list
}


type abc = {
  cpool: cpool;
  method_info:   method_info list;
  metadata:      int list;
  classes:       class_info list;
  instances:     instance_info list;
  script:        script list;
  method_body:   method_body list
}

(* cpool *)
val empty_cpool : cpool

(* serialize *)
val bytes_of_abc : abc -> Bytes.t list

(**{6 Debug only}*)

val bytes_of_cpool : cpool -> Bytes.t list
val bytes_of_method_info : method_info -> Bytes.t list
val bytes_of_script : script -> Bytes.t list
val bytes_of_trait : trait -> Bytes.t list
val bytes_of_method_body : method_body -> Bytes.t list

val bytes_of_class : class_info -> Bytes.t list
val bytes_of_instance : instance_info -> Bytes.t list
