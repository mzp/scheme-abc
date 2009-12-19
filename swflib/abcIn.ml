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
    match kind with
	  0x08 ->
	    Namespace name
	| 0x16 ->
	    PackageNamespace name
	| 0x17 ->
	    PackageInternalNamespace name
	| 0x18 ->
	    ProtectedNamespace name
	| 0x19 ->
	    ExplicitNamespace name
	| 0x1A ->
	    StaticProtectedNamespace name
	| 0x05 ->
	    PrivateNamespace name
	| _ ->
	    failwith "must not happen"

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
	| 0x0D ->
	    let ns =
	      u30 stream in
	    let name=
	      u30 stream in
	      QNameA (ns,name)
	| 0x0F ->
	    RTQName (u30 stream)
	| 0x10 ->
	    RTQNameA (u30 stream)
	| 0x11 ->
	    RTQNameL
	| 0x12 ->
	    RTQNameLA
	| 0x09 ->
	    let name=
	      u30 stream in
	    let ns_set=
	      u30 stream in
	      Multiname (name,ns_set)
	| 0x0E ->
	    let name=
	      u30 stream in
	    let ns_set=
	      u30 stream in
	      MultinameA (name,ns_set)
	| 0x1B ->
	    MultinameL (u30 stream)
	| 0x1C ->
	    MultinameLA (u30 stream)
	| _ ->
	    failwith "invalid format"

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
	    IntVal value
	| 0x04 ->
	    UIntVal value
	| 0x06 ->
	    DoubleVal value
	| 0x01 ->
	    StringVal value
	| 0x0B ->
	    BoolVal true
	| 0x0A ->
	    BoolVal false
	| 0x0C ->
	    NullVal
	| 0x00 ->
	    UndefinedVal
	| 0x08 ->
	    NamespaceVal value
	| 0x16  ->
	    PackageNamespaceVal value
	| 0x17 ->
	    PackageInternalNamespaceVal value
	| 0x18 ->
	    ProtectedNamespaceVal value
	| 0x19 ->
	    ExplicitNamespaceVal value
	| 0x1A ->
	    StaticProtectedNamespaceVal value
	| 0x05 ->
	    PrivateNamespaceVal value
	| _ ->
	    failwith "invalid format"

  let option_info stream =
    array option_detail stream

  let has x y =
    x land y = y

  let ifhas x y v =
    if has x y then [v] else []

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
    let options =
      if has flags 0x08 then
	[HasOptional (option_info stream)]
      else
	[] in
    let param_names =
      if has flags 0x80 then
	[HasParamNames (repeat param_count u30 stream)]
      else
	[] in
      {
	params       = param_types;
	return       = return_type;
	method_name  = name;
	method_flags = List.concat [
	  ifhas flags 0x01 NeedArguments;
	  ifhas flags 0x02 NeedActivation;
	  ifhas flags 0x04 NeedRest;
	  ifhas flags 0x40 SetDxns;
	  options; param_names;
	]
      (*       need_arguments  = has flags 0x01;
       need_activation =
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

  let to_metadata stream =
    let metadata_name =
      u30 stream in
    let items =
      array item_info stream in
      { metadata_name; items}

  (* 4.8 Traits *)
  let to_trait stream =
    let name =
      u30 stream in
    let kind =
      u8 stream in
    let attr =
      kind lsr 4 in
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
	    let attrs = List.concat [
	      if has attr 0x01 then [ATTR_Final] else [];
	      if has attr 0x02 then [ATTR_Override] else [];
	    ] in
	      begin match k with
		  1 -> MethodTrait (disp_id,methodi,attrs)
		| 2 -> GetterTrait (disp_id,methodi,attrs)
		| 3 -> SetterTrait (disp_id,methodi,attrs)
		| _ -> failwith "must not happen"
	      end
	| _ ->
	    failwith "invalid format" in
    let metadata =
      if has attr 0x4 then
	array u30 stream
      else
	[] in
      {
	trait_name    = name;
	data          = data;
	trait_metadata = metadata
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
	interfaces = interface;
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
    let cinit =
      u30 stream in
    let class_traits =
      array to_trait stream in
      { cinit; class_traits}

  (* 4.10 Script*)
  let to_script stream =
    let init =
      u30 stream in
    let script_traits =
      array to_trait stream in
      { init; script_traits}

  (* 4.12 Exception *)
  let to_exception stream =
    let from_pos = u30 stream in
    let to_pos   = u30 stream in
    let target   = u30 stream in
    let exception_type = u30 stream in
    let var_name = u30 stream in
      { from_pos; to_pos; target; exception_type; var_name }

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
      array to_exception stream in
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
      array to_metadata stream in
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
