open Base
open Byte 
open ExtString

let array f stream =
  let n =
    u30 stream in
    Parsec.repeat_l n f stream

let carray f stream =
  let n =
    u30 stream in
    Parsec.repeat_l (Int32.sub n 1l) f stream

(* constant pool *)
let string_info stream =
  let cs = 
    List.map char_of_int @@ array u8 stream in
    String.implode cs
    
let namespace_info stream =
  let kind =
    u8 stream in
  let name =
    u30 stream in
    match kind with
	0x08 ->
	  `Namespace name
      | 0x16 ->
	  `PackageNamespace name
      | 0x17 ->
	  `PackageInternaNs name
      | 0x18 ->
	  `ProtectedNamespace name
      | 0x19 ->
	  `ExplicitNamespace name
      | 0x1A ->
	  `StaticProtectedNs name
      | 0x05 ->
	  `PrivateNs name
      | _ ->
	  failwith "must not happen"

let ns_set_info stream =
  {| ns = array u30 stream |}

let multiname_info stream =
  let kind =
    u8 stream in
    match kind with
	0x07 ->
	  `QName {| ns=u30 stream; name=u30 stream |}
      | 0x0D ->
	  `QNameA {| ns=u30 stream; name=u30 stream |}
      | 0x0F ->
	  `RTQName {| name=u30 stream |}
      | 0x10 ->
	  `RTQNameA {| name=u30 stream |}
      | 0x11 ->
	  `RTQNameL
      | 0x12 ->
	  `RTQNameLA
      | 0x09 ->
	  `Multiname {|name=u30 stream; ns_set=u30 stream|}
      | 0x0E ->
	  `MultinameA {|name=u30 stream; ns_set=u30 stream|}
      | 0x1B ->
	  `MultinameL {|ns_set=u30 stream|}
      | 0x1C ->
	  `MultinameLA {|ns_set=u30 stream|}
      | _ ->
	  failwith "invalid format"

let constant_pool stream =
  {| integer  =carray s32 stream;
     uinteger =carray u32 stream;
     double   =carray d64 stream;
     string   =carray string_info stream;
     namespace=carray namespace_info stream;
     ns_set   =carray ns_set_info stream;
     multiname=carray multiname_info stream
  |}

(* method info *)
let option_detail stream =
  let value =
    u30 stream in
    match u8 stream with
	0x03 ->
	  `Int value
      | 0x04 ->
	  `UInt value
      | 0x06 ->
	  `Double value
      | 0x01 ->
	  `String value
      | 0x0B ->
	  `Bool true
      | 0x0A ->
	  `Bool false
      | 0x0C ->
	  `Null
      | 0x00 ->
	  `Undefined
      | 0x08 | 0x16 | 0x17 | 0x18 | 0x19 | 0x1A | 0x05 ->
	  `Namespace value
      | _ ->
	  failwith "invalid format"

let option_info stream =
  array option_detail stream

let has x y =
  x land y = y

let method_info stream =
  let param_count =
    u30 stream in
  let return_type =
    u30 stream in
  let param_types =
    Parsec.repeat_l param_count u30 stream in
  let name =
    u30 stream in
  let flags =
    u8 stream in
  let options =
    if has flags 0x08 then 
      Some (option_info stream )
    else 
      None in
  let param_names = 
    if has flags 0x80 then
      Some (Parsec.repeat_l param_count u30 stream)
    else 
      None in
    {| param_types     = param_types;
       return_type     = return_type;
       name            = name;
       need_arguments  = has flags 0x01;
       need_activation = has flags 0x02;
       need_rest       = has flags 0x04;
       set_dxns        = has flags 0x40;
       options         = options;
       param_names     = param_names
    |}

(* metadata *)
let item_info stream =
  {| key=u30 stream; 
     value=u30 stream |}

let metadata_info stream =
  {| name  = u30 stream;
     items = array item_info stream
  |}

(* 4.8 Traits *)
let trait_info stream =
  let name =
    u30 stream in
  let kind =
    u8 stream in
  let data =
    match kind land 0x0F with
	0 | 6 ->
	  let slot_id   = u30 stream in
	  let type_name = 
	    u30 stream in
	  let vindex =
	    u30 stream in
	  let vkind =
	    if vindex = 0l then
	      None
	    else
	      Some (u8 stream) in
	  let body =
	    {| slot_id=slot_id; type_name=type_name; vindex=vindex; vkind=vkind|} in
	    if kind = 0 then
	      `Slot body
	    else
	      `Const body
      | 4 ->
	  `Class {| slot_id=u30 stream; classi=u30 stream |}
      | 5 ->
	  `Function {| slot_id=u30 stream; functioni=u30 stream |}
      | 1 | 2 | 3 as k ->
	  let body =
	    {| disp_id=u30 stream; methodi=u30 stream |} in
	    begin match k with
		1 -> `Method body
	      | 2 -> `Getter body
	      | 3 -> `Setter body
	      | _ -> failwith "must not happen"
	    end
      | _ ->
	  failwith "invalid format" in
  let attr =
    kind lsr 4 in
  let metadata =
    if has attr 0x4 then
      Some (array u30 stream)
    else
      None in
    {| name          = name;
       data          = data;
       metadata      = metadata;
       attr_final    = has attr 0x01;
       attr_override = has attr 0x02
    |}

(* 4.7 Instance *)
let instance_info stream =
  let name = 
    u30 stream in
  let super_name =
    u30 stream in
  let flags =
    u8 stream in
  let protectedNs =
    if has flags 0x08 then 
      Some (u30 stream)
    else 
      None in
  let interface =
    array u30 stream in
  let iinit =
    u30 stream in
  let traits =
    array trait_info stream in
    {| name         = name;
       super_name   = super_name;
       is_sealed    = has flags 0x01;
       is_final     = has flags 0x02;
       is_interface = has flags 0x04;
       is_protected = has flags 0x08;
       protectedNs  = protectedNs;
       interface    = interface;
       iinit        = iinit;
       traits       = traits
    |}

(* 4.9 Class *)
let class_info stream =
  {| cinit = u30 stream; traits = array trait_info stream|}

(* 4.10 Script*)
let script_info stream =
  {| init = u30 stream; traits = array trait_info stream |}

(* 4.12 Exception *)
let exception_info stream =
  {| from_pos = u30 stream;
     to_pos   = u30 stream;
     target   = u30 stream;
     exc_type = u30 stream;
     var_name = u30 stream
  |}

(* 4.11 Method body *)
let method_body_info stream =
  let methodi =
    u30 stream in
  let max_stack =
    u30 stream in
  let local_count =
    u30 stream in
  let init_scope_depth =
    u30 stream in
  let max_scope_depth =
    u30 stream in
  let code =
    array u8 stream in
  let exceptions =
    array exception_info stream in
  let traits =
    array trait_info stream in
    {| methodi          = methodi;
       max_stack        = max_stack;
       local_count      = local_count;
       init_scope_depth = init_scope_depth;
       max_scope_depth  = max_scope_depth;
       code             = Parsec.many Disasm.code @@ Stream.of_list code;
       exceptions       = exceptions;
       traits           = traits
    |}
    
(* 4.2 ABC File *)
let abcFile stream =
  let minor_version = 
    u16 stream in
  let major_version = 
    u16 stream in
  let constant_pool = 
    constant_pool stream in
  let methods = 
    array method_info stream in
  let metadata =
    array metadata_info stream in
  let class_count =
    u30 stream in
  let instances =
    Parsec.repeat_l class_count instance_info stream in
  let classes = 
    Parsec.repeat_l class_count class_info stream in
  let script =
    array script_info stream in
  let method_body =
    array method_body_info stream in
    {| minor_version = minor_version;
       major_version = major_version;
       constant_pool = constant_pool;
       methods       = methods;
       metadata      = metadata;
       instances     = instances;
       classes       = classes;
       script        = script;
       method_body   = method_body
    |}

let of_stream =
  abcFile

let sample () =
  let ch = 
    open_in_bin "a.abc" in
    abcFile @@ Byte.of_channel ch
