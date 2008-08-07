
open Base
open Bytes

type namespace = {
  kind:int; ns_name:int
}
type namespace_set = int list

type multiname = 
    QName of int*int 
  | Multiname of int*int

(** AVM2 Overview: 4.3 Constant pool *)
type cpool = {
  int:           int list;
  uint:          int list;
  double:        float list;
  string:        string list;
  namespace:     namespace list;
  namespace_set: namespace_set list;
  multiname:     multiname list;
}

type method_info = {
  params: int list;
  return: int;
  name:   int;
  flags:  int;
}

type trait_data =
    SlotTrait of int * int * int * int
  | MethodTrait of int * int
  | GetterTrait of int * int
  | SetterTrait of int * int
  | ClassTrait  of int * int
  | FunctionTrait of int * int
  | ConstTrait of int * int * int * int

type trait = {t_name:int; data:trait_data}

type script = {
  init: int;
  trait_s: trait list
}

(** AVM2 Overview: 4.9 Class *)
type class_info = {
  cinit: int;
  trait_c: trait list
}

(** AVM2 Overview: 4.7 Instance *)
type instance_info={
  name_i:      int;
  super_name:  int;
  flags_c:     int;
  protectedNs: int;
  interface:   int list;
  iinit:       int;
  trait_i:     trait list
}

type method_body = {
  method_sig: int;
  max_stack: int;
  local_count: int;
  init_scope_depth: int;
  max_scope_depth: int;
  code: Bytes.t list;
  exceptions: int list;
  trait_m: trait list
}

(** AVM2 Overview: 4.2 abcFile *)
type abc = {
  cpool: cpool;
  method_info:   method_info list;
  metadata:      int list;
  classes:       class_info list;
  instances:     instance_info list;
  script:        script list;
  method_body:   method_body list
}

(** cpool **)
let empty_cpool = 
  { int=[]; uint=[]; double=[]; string=[]; namespace=[]; namespace_set=[]; multiname=[]}

(** create dummy list *)
let bytes_of_list _ = [u30 0]

let bytes_map f xs = 
  let ys = 
    HList.concat_map f xs in
    (u30 (List.length xs))::ys

(** encode for cpool *)

let cpool_map f xs = 
  let ys = 
    HList.concat_map f xs in
  let size =
    1+ List.length xs in
    (u30 size)::ys

let bytes_of_string str =
  bytes_map (fun c -> [u8 (Char.code c)]) @@ ExtString.String.explode str

let bytes_of_ns {kind=kind;ns_name=name} =
  [u8 kind; u30 name]

let bytes_of_ns_set =
  bytes_map (fun ns->[u30 ns])

let bytes_of_multiname =
  function 
      QName (ns,name) ->
	[u8 0x07;u30 ns; u30 name]
    | Multiname (name,ns_set) ->
	[u8 0x09;u30 name; u30 ns_set]

let bytes_of_cpool cpool = 
  List.concat [
    cpool_map (fun x->[s32 x]) cpool.int;
    cpool_map (fun x->[u32 x]) cpool.uint;
    cpool_map (fun x->[d64 x]) cpool.double;
    cpool_map bytes_of_string    cpool.string;
    cpool_map bytes_of_ns        cpool.namespace;
    cpool_map bytes_of_ns_set    cpool.namespace_set;
    cpool_map bytes_of_multiname cpool.multiname;
  ]

(* tairt *)
let bytes_of_trait_body =
  function
    SlotTrait (slot_id,type_name,vindex,vkind) ->
      if vindex = 0 then
	[u8 0;u30 slot_id; u30 type_name;u30 0]
      else
	[u8 0;u30 slot_id; u30 type_name;u30 vindex;u8 vkind]
  | MethodTrait (disp_id,meth) ->
      [u8 1;u30 disp_id; u30 meth]
  | GetterTrait (disp_id,meth) ->
      [u8 2;u30 disp_id; u30 meth]
  | SetterTrait (disp_id,meth) ->
      [u8 3;u30 disp_id; u30 meth]
  | ClassTrait  (slot_id,classi) ->
      [u8 4; u30 slot_id; u30 classi]
  | FunctionTrait (slot_id,func) ->
      [u8 5;u30 slot_id; u30 func]
  | ConstTrait (slot_id,type_name,vindex,vkind) ->
      if vindex = 0 then
	[u8 6;u30 slot_id; u30 type_name;u30 0]
      else
	[u8 6;u30 slot_id; u30 type_name;u30 vindex;u8 vkind]

let bytes_of_trait {t_name=name; data=data} =
  List.concat [[u30 name];
	       bytes_of_trait_body data;
	       [u30 0]] (* skip metadata *)

(* other *)
let bytes_of_method_info info =
  List.concat [[u30 (List.length info.params);
		u30 info.return];
	       List.map u30 info.params;
	       [u30 info.name;
		u8  info.flags]]

let bytes_of_script script =
  (u30 script.init)::bytes_map bytes_of_trait script.trait_s

let bytes_of_method_body body = 
  List.concat [
    [ u30 body.method_sig;
      u30 body.max_stack;
      u30 body.local_count;
      u30 body.init_scope_depth;
      u30 body.max_scope_depth;
      block body.code];
    bytes_of_list body.exceptions;
    bytes_map bytes_of_trait body.trait_m]

let bytes_of_class  {cinit=init; trait_c=traits} =
  List.concat [
    [u30 init];
    bytes_map bytes_of_trait traits]

let bytes_of_instance {name_i      = name;
		       super_name  = sname;
		       flags_c     = flags;
		       protectedNs = pns;
		       interface   = inf;
		       iinit       = init;
		       trait_i     = traits} =
  List.concat [
    [u30 name;
     u30 sname;
     u8  flags;
     u30 pns];
    bytes_map (fun x -> [u30 x]) inf;
    [u30 init];
    bytes_map bytes_of_trait traits]


let bytes_of_abc { cpool=cpool;
		   method_info=info;
		   metadata=metadata;
		   classes=classes;
		   instances=instances;
		   script=script;
		   method_body=body; } =
  List.concat [
    [ u16 16; u16 46; ]; (* version *)
    bytes_of_cpool cpool;
    bytes_map bytes_of_method_info info;
    bytes_of_list metadata;
    (* todo: instances *)
    bytes_map bytes_of_class classes;
    HList.concat_map  bytes_of_instance instances;
    bytes_map bytes_of_script script;
    bytes_map bytes_of_method_body body
  ]

let test () = 
  let cpool = 
    empty_cpool in
  let info =
    { params=[]; return=0; name=0; flags=0 } in
  let script  =
    { init=0; trait_s=[]} in
  let body =
    { method_sig=0; max_stack=2; local_count=1; init_scope_depth=0; max_scope_depth=1;
      code= [ u8 0xD0; (* getlocal0 *)
	      u8 0x30; (* pushscope *)
	      u8 0x29;
	      u8 0x47 ];
      exceptions=[]; trait_m=[] } in
  let abc =
    {cpool=cpool; method_info=[info]; metadata=[]; classes=[]; instances=[]; 
     script=[script]; method_body=[body]} in
  let ch = open_out_bin "abc.abc" in
    output_bytes ch @@ bytes_of_abc abc;
    close_out ch;
    abc
