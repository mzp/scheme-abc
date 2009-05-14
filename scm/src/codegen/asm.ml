open Base
open Bytes

include Instruction

type t = {
  abc_cpool:     Abc.cpool;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}

let fork2 f g x       = (f x, g x)
let fork3 f g h x     = (f x, g x, h x)
let fork4 f g h i x   = (f x, g x, h x, i x)

let with2 f g (a,b) = (f a, g b)
let with3 f g h (a,b,c) = (f a, f b, h c)
let with4 f g h i (a,b,c,d) = (f a, f b, h c, i d)

let join2 f (a,b)     = f a b
let join3 f (a,b,c)   = f a b c
let join4 f (a,b,c,d) = f a b c d

(*
         +-------------+
cpool -> |             |
         | const_cpool | -> cpool'
inst  -> |             |
         +-------------+

*)
let const_pool cpool inst =
  (spec inst).const
  +> List.fold_left (flip Cpool.add) cpool

(*
encode
*)
let encode cpool inst =
  let {op=op; args = args} =
    spec inst in
    u8 op::args { methods=[]; classes=[]; cpool=cpool}

(*
usage
*)
let add (y,m) x =
  (x+y,max (x+y) m)

let stack inst =
  (spec inst).stack
let scope inst =
  (spec inst).scope
let count inst =
  (spec inst).count

let calc f current inst =
  inst
  +> f
  +> add current

let empty_usage =
  {| stack=(0,0); scope=(0,0) |}

let to_usage usage inst =
  inst
  +> fork2 (calc stack usage#stack) (calc scope usage#scope)
  +> join2 (fun stack scope ->  {| stack=stack; scope=scope |})

let to_byte cpool inst =
  inst
  +> fork2 (const_pool cpool) id
  +> fork2 fst (curry encode)

let asm_inst (cpool,usage) inst =
  inst
  +> fork2 (to_byte cpool) (to_usage usage)
  +> (fun ((cpool,bytes),info) -> ((cpool,usage),bytes))

let asm_method cpool m =
  let (cpool',usage), code =
    m.instructions
    +> map_accum_left asm_inst (cpool,empty_usage)
    +> with2 (with2 (Cpool.add (m.method_name :> Cpool.entry)) id) List.concat in
  let info =
    { Abc.params       = m.params;
      return           = m.return;
      method_name      = Cpool.index (m.method_name :> Cpool.entry) cpool';
      method_flags     = m.method_flags } in
  let body =
    { Abc.method_sig   = 0;
      max_stack        = snd usage#stack;
      local_count      = List.length m.params+1;
      init_scope_depth = 0;
      max_scope_depth  = snd usage#scope;
      code             = code;
      exceptions       = [];
      method_traits    = [] } in
    cpool',(info,body)
