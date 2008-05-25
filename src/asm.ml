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

let method_asm cmap index m =
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
	       prefix cmap;
	       [Bytes.u8 op];
	       args cmap] in
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

let collect xs =
  List.fold_left Cpool.append Cpool.empty @@
    List.map (fun x->(get_config x).const) xs

let assemble methods =
  let cmap = 
    Cpool.to_cmap
    @@ List.fold_left Cpool.append Cpool.empty
    @@ List.map (fun {instructions=inst}->collect inst) methods in
  let cpool =
    Cpool.to_cpool cmap in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> method_asm cmap i x) methods in
    cpool,info,body
