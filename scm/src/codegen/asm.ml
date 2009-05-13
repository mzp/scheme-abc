open Base;;

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

let encode cpool inst =
  let {op=op;args = args} =
    spec inst in
    u8 op::args { methods=[]; classes=[]; cpool=cpool}


let asm_inst cpool inst =
  inst
  +> fork2 (const_pool cpool) id
  +> fork2 fst (curry encode)

let asm_method cpool xs =
  map_accum_left asm_inst cpool xs

