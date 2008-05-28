
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
}

val assemble : meth -> Abc.cpool * Abc.method_info list * Abc.method_body list

