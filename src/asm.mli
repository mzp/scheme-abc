type klass_type = Sealed | Final | Interface | ProtectedNs of Cpool.namespace

type instruction =
#include "opcode.ml"
 and meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
} and klass = {
   cname:     string;
   sname:     string;
   flags_k:   klass_type list;
   cinit:     meth;
   iinit:     meth;
   interface: klass list;
   methods:   meth list
}

type t = {
  abc_cpool:     Abc.cpool;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}

val make_meth: ?args:int list -> string -> instruction list -> meth
val assemble : meth -> t

(**{6 For debug}*)

val string_of_instruction : instruction -> string
val collect_method : meth -> meth list
val collect_const  : meth -> Cpool.t

