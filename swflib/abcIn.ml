open Base
open BytesIn
open ExtString

module type Inst = sig
  type t
  val of_bytes : BytesIn.t Stream.t -> t
end

let cMajorVersion = 46
let cMinorVersion = 16

module Make(Inst : Inst) = struct
  open AbcType

  let rec repeat n f stream =
    if n <= 0 then
      []
    else
      match stream with parser
	  [<c = f>] ->
	    c::repeat (n-1) f stream
	| [<>] ->
	    raise (Stream.Error "invalid format")

  let rec many parse stream =
    match stream with parser
	[< e = parse; s>] -> e::many parse s
      | [<>] -> []

  let array f stream =
    let n =
      u30 stream in
      repeat n f stream

  let carray f stream =
    let n =
      u30 stream in
      repeat (n-1) f stream

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
      {kind=kind; namespace_name=name}
(* TDOO *)
(*      match kind with
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
*)
  let ns_set_info stream =
    array u30 stream

  let multiname_info stream =
    let kind =
      u8 stream in
      match kind with
	  0x07 ->
	    let ns =
	      u30 stream in
	    let name=
	      u30 stream in
	    QName (ns,name)
	| 0x09 ->
	    let name =
	      u30 stream in
	    let ns_set =
	      u30 stream in
	    Multiname (name,ns_set)
	| _ ->
	    failwith "invalid format"
(* TODO *)
(*      match kind with
	  0x07 ->
	    `QName { ns=u30 stream; name=u30 stream }
	| 0x0D ->
	    `QNameA { ns=u30 stream; name=u30 stream }
	| 0x0F ->
	    `RTQName { name=u30 stream }
	| 0x10 ->
	    `RTQNameA { name=u30 stream }
	| 0x11 ->
	    `RTQNameL
	| 0x12 ->
	    `RTQNameLA
	| 0x09 ->
	    `Multiname {name=u30 stream; ns_set=u30 stream}
	| 0x0E ->
	    `MultinameA {name=u30 stream; ns_set=u30 stream}
	| 0x1B ->
	    `MultinameL {ns_set=u30 stream}
	| 0x1C ->
	    `MultinameLA {ns_set=u30 stream}
	| _ ->
	    failwith "invalid format"
*)

  let to_cpool stream =
    let int =
      List.map (Int32.to_int) @@ carray s32 stream in
    let uint =
      List.map (Int32.to_int) @@ carray u32 stream in
    let double=
      carray d64 stream in
    let string =
      carray string_info stream in
    let namespace=
      carray namespace_info stream in
    let namespace_set =
      carray ns_set_info stream in
    let multiname     =
      carray multiname_info stream in
    {
      int; uint; double; string; namespace; namespace_set; multiname;
    }

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

  let to_method_info stream =
    let param_count =
      u30 stream in
    let return_type =
      u30 stream in
    let param_types =
      repeat param_count u30 stream in
    let name =
      u30 stream in
    let flags =
      u8 stream in
(*    let options =
      if has flags 0x08 then
	Some (option_info stream )
      else
	None in
    let param_names =
      if has flags 0x80 then
	Some (repeat param_count u30 stream)
      else
	None in*)
      { params     = param_types;
       return     = return_type;
	method_name            = name;
	method_flags = flags
      (*       need_arguments  = has flags 0x01;
       need_activation = has flags 0x02;
       need_rest       = has flags 0x04;
       set_dxns        = has flags 0x40;
       options         = options;
       param_names     = param_names*)
      }

  (* metadata *)
  let item_info stream =
    let key=u30 stream in
    let value=u30 stream in
      (key,value)

  let metadata_info stream =
    {
      metadata_name  = u30 stream;
      items = array item_info stream
    }

  (* 4.8 Traits *)
  let to_trait stream =
    let name =
      u30 stream in
    let kind =
      u8 stream in
    let data =
      match kind land 0x0F with
	  0 | 6 ->
	    let slot_id   =
	      u30 stream in
	    let type_name =
	      u30 stream in
	    let vindex =
	      u30 stream in
	    let vkind =
	      if vindex = 0 then
		0
	      else
		u8 stream in
	      if kind = 0 then
		SlotTrait(slot_id,type_name,vindex,vkind)
	      else
		ConstTrait(slot_id,type_name,vindex,vkind)
	| 4 ->
	    let id =
	      u30 stream in
	    let classi =
	      u30 stream in
	      ClassTrait(id,classi)
	| 5 ->
	    let slot_id=
	      u30 stream in
	    let functioni=
	      u30 stream in
	    FunctionTrait (slot_id,functioni)
	| 1 | 2 | 3 as k ->
	    let disp_id=
	      u30 stream in
	    let methodi=
	      u30 stream in
	    let flag =
	      kind lsr 4 in
	    let attrs = List.concat [
	      if has flag 0x01 then [ATTR_Final] else [];
	      if has flag 0x02 then [ATTR_Override] else [];
	      if has flag 0x04 then [ATTR_Medadata] else [];
	    ] in
	      begin match k with
		  1 -> MethodTrait (disp_id,methodi,attrs)
		| 2 -> GetterTrait (disp_id,methodi,attrs)
		| 3 -> SetterTrait (disp_id,methodi,attrs)
		| _ -> failwith "must not happen"
	      end
	| _ ->
	    failwith "invalid format" in
(* TODO *)
(*    let metadata =
      if has attr 0x4 then
	Some (array u30 stream)
      else
	None in*)
      {
	trait_name    = name;
	data          = data;
      }

  (* 4.7 Instance *)
  let to_instance stream =
    let name =
      u30 stream in
    let super_name =
      u30 stream in
    let flags =
      u8 stream in
    let protectedNs =
      if has flags 0x08 then
	[ProtectedNs (u30 stream)]
      else
	[] in
    let interface =
      array u30 stream in
    let iinit =
      u30 stream in
    let traits =
      array to_trait stream in
      { instance_name = name;
	super_name = super_name;
	interface = interface;
	iinit           = iinit;
	instance_traits = traits;
	instance_flags = List.concat [
	  if has flags 0x01 then [Sealed] else [];
	  if has flags 0x02 then [Final] else [];
	  if has flags 0x04 then [Interface] else [];
	  protectedNs]
      }

  (* 4.9 Class *)
  let to_class stream =
    { cinit = u30 stream; class_traits = array to_trait stream}

  (* 4.10 Script*)
  let to_script stream =
    { init = u30 stream; script_traits = array to_trait stream }

  (* 4.12 Exception *)
  let exception_info stream =
    {
      from_pos = u30 stream;
      to_pos   = u30 stream;
      target   = u30 stream;
      exception_type = u30 stream;
      var_name = u30 stream
    }

  (* 4.11 Method body *)
  let to_method_body stream =
    let method_sig =
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
      array to_trait stream in
      {
	method_sig;
	max_stack;
	local_count;
	init_scope_depth;
	max_scope_depth;
	code             = many Inst.of_bytes @@ BytesIn.of_list code;
	exceptions;
	method_traits = traits
      }

  (* 4.2 ABC File *)
  let abcFile stream =
    let _ =
      assert (cMinorVersion = u16 stream) in
    let _ =
      assert (cMajorVersion = u16 stream) in
    let cpool =
      to_cpool stream in
    let method_info =
      array to_method_info stream in
    let metadata =
      array metadata_info stream in
    let class_count =
      u30 stream in
    let instances =
      repeat class_count to_instance stream in
    let classes =
      repeat class_count to_class stream in
    let scripts =
      array to_script stream in
    let method_bodies =
      array to_method_body stream in
      {
	cpool; method_info; metadata;
	instances; classes; scripts; method_bodies;
      }

  let of_bytes stream =
    abcFile stream
end
