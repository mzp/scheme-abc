open Base
open Bytes

(* ----------------------------------------
   Type
   ---------------------------------------- *)
type namespace = {
  kind:int; ns_name:int
}
type namespace_set = int list

type multiname = 
    QName of int*int 
  | Multiname of int*int


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

type trait =
    {t_name:int; data:trait_data}

type script = {
  init: int;
  trait_s: trait list
}

type class_info = {
  cinit: int;
  trait_c: trait list
}

type class_flag = 
    Sealed | Final | Interface | ProtectedNs of int

type instance_info={
  name_i:      int;
  super_name:  int;
  flags_i:     class_flag list;
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

type abc = {
  cpool: cpool;
  method_info:   method_info list;
  metadata:      int list;
  classes:       class_info list;
  instances:     instance_info list;
  script:        script list;
  method_body:   method_body list
}

(* ----------------------------------------
   Utils
   ---------------------------------------- *)
let dummy _ = [u30 0]

let array f xs = 
  let ys = 
    HList.concat_map f xs in
    (u30 (List.length xs))::ys

(* ----------------------------------------
   Constant Pool
   ---------------------------------------- *)
let empty_cpool = 
  { int=[]; uint=[]; double=[]; string=[]; namespace=[]; namespace_set=[]; multiname=[]}

let cpool_map f xs = 
  let ys = 
    HList.concat_map f xs in
  let size =
    1+ List.length xs in
    (u30 size)::ys

let of_string str =
  array (fun c -> [u8 (Char.code c)]) @@ ExtString.String.explode str

let of_ns {kind=kind;ns_name=name} =
  [u8 kind; u30 name]

let of_ns_set =
  array (fun ns->[u30 ns])

let of_multiname =
  function 
      QName (ns,name) ->
	[u8 0x07;u30 ns; u30 name]
    | Multiname (name,ns_set) ->
	[u8 0x09;u30 name; u30 ns_set]

let of_cpool cpool = 
  List.concat [
    cpool_map (fun x->[s32 x]) cpool.int;
    cpool_map (fun x->[u32 x]) cpool.uint;
    cpool_map (fun x->[d64 x]) cpool.double;
    cpool_map of_string    cpool.string;
    cpool_map of_ns        cpool.namespace;
    cpool_map of_ns_set    cpool.namespace_set;
    cpool_map of_multiname cpool.multiname;
  ]

(* ----------------------------------------
   Trait
   ---------------------------------------- *)
let of_trait_body =
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

let of_trait {t_name=name; data=data} =
  List.concat [[u30 name];
	       of_trait_body data]

(* ----------------------------------------
   Other
   ---------------------------------------- *)
let of_method_info info =
  List.concat [[u30 (List.length info.params);
		u30 info.return];
	       List.map u30 info.params;
	       [u30 info.name;
		u8  info.flags]]

let of_script script =
  (u30 script.init)::array of_trait script.trait_s

let of_method_body body = 
  List.concat [
    [ u30 body.method_sig;
      u30 body.max_stack;
      u30 body.local_count;
      u30 body.init_scope_depth;
      u30 body.max_scope_depth;
      block body.code];
    dummy body.exceptions;
    array of_trait body.trait_m]

let of_class  {cinit=init; trait_c=traits} =
  List.concat [
    [u30 init];
    array of_trait traits]

let of_instance {name_i      = name;
		       super_name  = sname;
		       flags_i     = flags;
		       interface   = inf;
		       iinit       = init;
		       trait_i     = traits} =
  let flag = function
      Sealed        -> 0x01 
    | Final         -> 0x02
    | Interface     -> 0x04
    | ProtectedNs _ -> 0x08 in
  let flags' =
    List.fold_left (fun x y -> x lor (flag y)) 0 flags in
  let ns =
    try
      match List.find (function ProtectedNs _ -> true | _ -> false) flags with
	  ProtectedNs ns ->
	    [u30 ns]
	| _ ->
	    failwith "must not happen"
    with Not_found -> 
      [] in
    List.concat [
      [u30 name;
       u30 sname;
       u8  flags'];
      ns;
      array (fun x -> [u30 x]) inf;
      [u30 init];
      array of_trait traits]


let to_bytes { cpool=cpool;
	       method_info=info;
	       metadata=metadata;
	       classes=classes;
	       instances=instances;
	       script=script;
	       method_body=body; } =
  List.concat [
    [ u16 16; u16 46; ];
    of_cpool cpool;
    array of_method_info info;
    dummy metadata;
    array of_instance instances;
    HList.concat_map of_class classes;
    array of_script script;
    array of_method_body body
  ]
