open Base
open Cpool
open Bytes

module Set = Core.Std.Set
type 'a set = 'a Set.t

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
  name:  Cpool.multiname;
  sname: Cpool.multiname;
  cinit: meth;
  iinit: meth;
  methods: meth;
  interface: Cpool.multiname
}


type mmap = meth list
type config = {
  op:     int;
  args:   Cpool.t * mmap -> Bytes.t list;
  prefix: Cpool.t * mmap -> Bytes.t list;
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

#include "match.ml"

let make_meth ?(args=[]) name body = 
  let inst =
      body @
      [ReturnValue] in
  { name  = name;
    params= args;
    return=0;
    flags =0;
    exceptions=[];
    traits=[];
    instructions=inst}

let rec fold_method f init m =
  let branch x inst =
    match (get_config inst).meth with
	Some meth ->
	  fold_method f (f x meth) meth
      |	None ->
	  x in
    f (List.fold_left branch init m.instructions) m

let collect_const =
  fold_method
    (fun cpool {instructions=insts}->
       List.fold_left (fun cpool' i-> Cpool.append cpool' (get_config i).const) cpool insts)
    Cpool.empty

let collect_method =
  Set.to_list $ fold_method (flip Set.add) Set.empty

(* convert instruction *)
let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let asm_method map index m =
  let configs =
    List.map get_config m.instructions in
  let zero =
    (0,0) in
  let (max_stack,_),(max_scope,_),local_count,bytes = 
    List.fold_left
      (fun 
	 (stack,scope,count,bytes) 
	 {op=op;prefix=prefix;args=args;stack=st;scope=sc;count=c} -> 
	   let by =
	     List.concat [
	       prefix map;
	       [Bytes.u8 op];
	       args map] in
	     add stack st,add scope sc,max count c,by::bytes)
      (zero,zero,1,[]) configs in
  let info =
    { Abc.params=m.params; 
      Abc.return=m.return; 
      Abc.name=index; 
      Abc.flags=m.flags } in
  let body =
    { Abc.method_sig=index;
      Abc.max_stack=max_stack;
      Abc.local_count=List.length m.params+1;
      Abc.init_scope_depth=0; 
      Abc.max_scope_depth=max_scope;
      Abc.code=List.concat @@ List.rev bytes;
      Abc.exceptions=[]; 
      Abc.trait_m=[] } in
      info,body

let assemble meth =
  let meths =
    collect_method meth in
  let cpool = 
    collect_const meth in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> asm_method (cpool,meths) i x) meths in
    Cpool.to_abc cpool,info,body
