type script = {
  init: int;
  trait_s: int list
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
  trait_m: int list
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

type abc = {
  cpool: cpool;
  method_info:   method_info list;
  metadata:      int list;
  classes:       int list;
  instances:     int list;
  script:        script list;
  method_body:   method_body list
}

(* cpool *)
val empty_cpool : cpool

(* serialize *)
val bytes_of_abc : abc -> Bytes.t list
val bytes_of_cpool : cpool -> Bytes.t list
val bytes_of_method_info : method_info -> Bytes.t list
val bytes_of_script : script -> Bytes.t list
val bytes_of_method_body : method_body -> Bytes.t list
