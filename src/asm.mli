
type trait_body = Slot of int
type trait = string * trait_body

type instruction =
#include "opcode.ml"
 and meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: trait list;
  exceptions: int list;
}

val assemble : meth -> Abc.cpool * Abc.method_info list * Abc.method_body list

val string_of_instruction : instruction -> string
