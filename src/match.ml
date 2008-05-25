type config = {
  op:int;
  args: Cpool.cmap -> Bytes.t list;
  prefix: Cpool.cmap -> Bytes.t list;
  const:  Cpool.t;
  stack: int;
  scope: int;
  count: int;
}

let const x _ = x
let default = {
  op=0;
  args=const [];
  prefix=const [];
  const= Cpool.empty;
  stack=0;
  scope=0;
  count=0;
}
open Opcode.B
open Cpool
open Bytes

#include <match_core.ml>

