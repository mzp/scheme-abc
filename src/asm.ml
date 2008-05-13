open Base
open Cpool

type instruction = 
  | Add_i 
  | GetLocal 
  | PushScope 
  | ReturnVoid 
  | FindPropStrict of multiname 
  | PushString of string 
  | PushInt of int
  | PushUInt of int
  | CallPropLex of multiname * int
  | Pop

(* instruction config *)
type config = {
  op:int;
  args: cmap -> Bytes.t list;
  const:  clist;
  stack: int;
  scope: int;
  count: int;
}

type meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
}

let const x _ = x

(* instruction configure *)
let default = {
  op=0;
  args=const [];
  const= Cpool.empty;
  stack=0;
  scope=0;
  count=0;
}

let get_config = function
  | Add_i ->
      {default with op=0xc5; stack= ~-1}
  | GetLocal ->
      {default with op=0xD0; stack=1}
  | PushScope -> 
      {default with op=0x30; stack= ~-1; scope=1}
  | ReturnVoid -> 
      {default with op=0x47}
  | PushString str ->
      {default with op=0x2C; stack=1; const=string str; args=fun cmap -> [string_get str cmap];}
  | PushInt n ->
      {default with op=0x2D; stack=1; const=int n; args=fun cmap -> [int_get n cmap];}
  | PushUInt n ->
      {default with op=0x2E; stack=1; const=uint n; args=fun cmap -> [uint_get n cmap];}
  | FindPropStrict name ->
      {default with op=0x5D; stack=1; const=multiname name; args=fun cmap ->[multiname_get name cmap];}
  | Pop ->
      {default with op=0x29; stack= ~-1}
  | CallPropLex(name,count) ->
      {default with op=0x4c; stack= ~-count; args=fun cmap->[multiname_get name cmap;Bytes.U30 count]}

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
	 {op=op;args=args;stack=st;scope=sc;count=c} -> 
	   let by =
	     (Bytes.U8 op)::args cmap in
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
      instructions=[GetLocal;
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
