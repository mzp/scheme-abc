open Opcode.B
open Cpool
open Bytes

type config = {
  op:     int;
  args:   Cpool.cmap -> Bytes.t list;
  prefix: Cpool.cmap -> Bytes.t list;
  const:  Cpool.t;
  meth:   meth option;
  stack:  int;
  scope:  int;
  count:  int;
}

let const x _ = x
let default = {
  op=0;
  args=const [];
  prefix=const [];
  const= Cpool.empty;
  meth = None;
  stack=0;
  scope=0;
  count=0;
}

#include <match_core.ml>
