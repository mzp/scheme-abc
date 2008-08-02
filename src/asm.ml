open Base
open Cpool
open Bytes

module Set = Core.Std.Set
type 'a set = 'a Set.t


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

(* collect info for cpool *)
 let make_qname name = 
   Cpool.QName (Cpool.Namespace "",name)

let collect_traits xs = 
  List.fold_left Cpool.append Cpool.empty @@
    List.map (fun (name,_) -> Cpool.multiname (make_qname name)) xs

let rec collect ({instructions=insts;traits=traits} as meth) =
  let meth_and_const inst =
    let {meth=m;const=c} =
      get_config inst in
      match m with
	  Some child ->
	    let m',c' =
	      collect child in
	      m',Cpool.append c c'
	| None ->
	    Set.empty,c in
  let meths,consts =
    List.fold_left 
      (fun (m0,c0) (m,c) -> Set.union m m0,Cpool.append c c0) 
      (Set.empty,Cpool.empty) @@ List.map meth_and_const insts in
    Set.add meth meths,consts

(* convert instruction *)
let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let asm_trait (map,_) (name,trait) =
  let i = 
    Cpool.multiname_nget (make_qname name) map in
  let data = 
    match trait with
	Slot n -> Abc.SlotTrait (n,0,0,0) in
    {Abc.t_name=i;Abc.data=data}

let asm_method map index m =
  let configs =
    List.map get_config m.instructions in
  let init =
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
	     add stack st,add scope sc,(if count > c then count else c),by::bytes)
      (init,init,1,[]) configs in
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
      Abc.trait_m=List.map (asm_trait map) m.traits } in
      info,body

let assemble meth =
  let meths,cpool = 
    collect meth in
  let meths' =
    Set.to_list meths in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> asm_method (cpool,meths') i x) meths' in
    Cpool.to_abc cpool,info,body
