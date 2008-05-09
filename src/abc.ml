open Base
open Bytes

type namespace = {
  kind:int; ns_name:int
}
type namespace_set = int list

type multiname = 
    QName of int*int 
  | Multiname of int*int

type cpool = {
  int:    int list;
  uint:  int list;
  double: float list;
  string: string list;
  namespace: namespace list;
  namespace_set:    namespace_set list;
  multiname: multiname list;
}

type method_info = {
  params: int list;
  return: int;
  name:   int;
  flags:  int;
}

type script = {
  init: int;
  trait_s: int list
}

type method_body = {
  method_sig: int;
  max_stack: int;
  local_count: int;
  init_scope_depth: int;
  max_scope_depth: int;
  code: Bytes.t list;
  exceptions: int list;
  trait_m: int list
}

type abc = {
  cpool: cpool;
  method_info:   method_info list;
  metadata:      int list;
  classes:       int list;
  instances:     int list;
  script:        script list;
  method_body:   method_body list
}

(** cpool **)
let empty_cpool = 
  { int=[]; uint=[]; double=[]; string=[]; namespace=[]; namespace_set=[]; multiname=[]}

(** serialize **)
let bytes_of_list [] = [U30 0]

let bytes_map f xs = 
  let ys = 
    List.concat @@ List.map f xs in
    (U30 (List.length xs))::ys

let cpool_map f xs = 
  let ys = 
    List.concat @@ List.map f xs in
    (U30 ((List.length xs)+1))::ys

let bytes_of_string str =
  bytes_map (fun c -> [Bytes.U8 (Char.code c)]) @@ ExtString.String.explode str

let bytes_of_ns {kind=kind;ns_name=name} =
  [U8 kind; U30 name]

let bytes_of_ns_set =
  bytes_map (fun ns->[U30 ns])

let bytes_of_multiname =
  function 
      QName (ns,name) ->
	[U8 0x07;U30 ns; U30 name]
    | Multiname (name,ns_set) ->
	[U8 0x09;U30 name; U30 ns_set]

let bytes_of_cpool cpool = 
  List.concat @@ [
    bytes_of_list cpool.int;
    bytes_of_list cpool.uint;
    bytes_of_list cpool.double;
    cpool_map bytes_of_string    cpool.string;
    cpool_map bytes_of_ns        cpool.namespace;
    cpool_map bytes_of_ns_set    cpool.namespace_set;
    cpool_map bytes_of_multiname cpool.multiname;
  ]

let bytes_of_method_info info =
  [ U30 (List.length info.params);
    U30 info.return;
    (* info.params *)
    U30 info.name;
    U8  info.flags]

let bytes_of_script script =
  (U30 script.init)::bytes_of_list script.trait_s

let bytes_of_method_body body = 
  List.concat [
    [ U30 body.method_sig;
      U30 body.max_stack;
      U30 body.local_count;
      U30 body.init_scope_depth;
      U30 body.max_scope_depth ];
    U30 (List.length body.code)::body.code;
    bytes_of_list body.exceptions;
    bytes_of_list body.trait_m]

let bytes_of_abc { cpool=cpool;
		   method_info=info;
		   metadata=metadata;
		   classes=classes;
		   instances=instances;
		   script=script;
		   method_body=body; } =
  List.concat [
    [ U16 16; U16 46; ]; (* version *)
    bytes_of_cpool cpool;
    bytes_map bytes_of_method_info info;
    bytes_of_list metadata;
    (* todo: instances *)
    bytes_of_list classes;
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
      code= [ U8 0xD0; (* getlocal0 *)
	      U8 0x30; (* pushscope *)
	      U8 0x29;
	      U8 0x47 ];
      exceptions=[]; trait_m=[] } in
  let abc =
    {cpool=cpool; method_info=[info]; metadata=[]; classes=[]; instances=[]; 
     script=[script]; method_body=[body]} in
  let ch = open_out_bin "abc.abc" in
    output_bytes ch @@ bytes_of_abc abc;
    close_out ch;
    abc
