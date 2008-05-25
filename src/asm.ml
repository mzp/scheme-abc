open Base
open Cpool

include Opcode.B
include Match

(* convert instruction *)
let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let method_asm map index m =
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
	     add stack st,add scope sc,count+c,by::bytes)
      (init,init,1,[]) configs in
  let info =
    { Abc.params=m.params; 
      Abc.return=m.return; 
      Abc.name=index; 
      Abc.flags=m.flags } in
  let body =
    { Abc.method_sig=index;
      Abc.max_stack=max_stack;
      Abc.local_count=local_count;
      Abc.init_scope_depth=0; 
      Abc.max_scope_depth=max_scope;
      Abc.code=List.concat @@ List.rev bytes;
      Abc.exceptions=[]; 
      Abc.trait_m=[] } in
      info,body

let rec collect ({instructions=insts} as meth) =
  let meth_and_const inst =
    let {meth=m;const=c} =
      get_config inst in
      match m with
	  Some child ->
	    let m',c' =
	      collect child in
	      Pool.add child m',Cpool.append c c'
	| None ->
	    Pool.empty,c in
    List.fold_left (fun (m0,c0) (m,c) -> Pool.append m m0,Cpool.append c c0) (Pool.add meth Pool.empty,Cpool.empty) 
    @@ List.map meth_and_const insts

let assemble meth =
  let meths,consts = 
    collect meth in
  let cmap =
    Cpool.to_cmap consts in
  let cpool =
    Cpool.to_cpool cmap in
  let mmap =
    Pool.to_map meths in
  let meths' =
    Pool.to_list meths in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> method_asm (cmap,mmap) i x) meths' in
    cpool,info,body
