open Base
open Cpool

include Opcode.B
include Match

(* instruction config *)
type meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
}

(* instruction configure *)


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
    cmap_of_clist
    @@ List.fold_left Cpool.append Cpool.empty
    @@ List.map (fun {instructions=inst}->collect inst) methods in
  let cpool =
    cpool_of_cmap cmap in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> method_asm cmap i x) methods in
    cpool,info,body

let test () = 
  let m = 
    { name  ="main";
      params=[];
      return=0;
      flags =0;
      exceptions=[];
      traits=[];
      instructions=[GetLocal_0;
		    PushScope;
		    FindPropStrict (QName ((Namespace ""),"print"));
		    PushInt 42;
		    CallPropLex (QName ((Namespace ""),"print"),1);
		    Pop;
		    ReturnVoid]} in
  let cpool,info,body = 
    assemble [m] in
  let script  =
    { Abc.init=0; Abc.trait_s=[]} in
  let abc =
    {Abc.cpool=cpool; 
     Abc.method_info=info;
     Abc.method_body=body;
     Abc.metadata=[]; Abc.classes=[]; Abc.instances=[]; 
     Abc.script=[script]; } in
  let ch = open_out_bin "asm.abc" in
    Bytes.output_bytes ch @@ Abc.bytes_of_abc abc;
    close_out ch;
    abc
