open Base

module type Inst = sig
  type t
  val to_bytes : t -> BytesOut.t list
end

module Make(Inst : Inst) = struct
  open BytesOut
  open AbcType

  let dummy _ = [u30 0]

  let array ~f xs =
    let ys =
      HList.concat_map f xs in
      (u30 (List.length xs))::ys

  (* Constant Pool *)
  let empty_cpool =
    { int=[]; uint=[]; double=[]; string=[]; namespace=[]; namespace_set=[]; multiname=[]}

  let cpool_map f xs =
    let ys =
      HList.concat_map f xs in
    let size =
      1+ List.length xs in
      (u30 size)::ys

  let of_string str =
    array ~f:(fun c -> [u8 (Char.code c)]) @@ ExtString.String.explode str

  let of_ns =
    function
	Namespace name ->
	  [u8 0x08; u30 name]
      | PackageNamespace name ->
	  [u8 0x16; u30 name]
      | PackageInternalNamespace name ->
	  [u8 0x17; u30 name];
      | ProtectedNamespace name ->
	  [u8 0x18; u30 name]
      | ExplicitNamespace name ->
	  [u8 0x19; u30 name]
      | StaticProtectedNamespace name ->
	  [u8 0x1A; u30 name]
      | PrivateNamespace name ->
	  [u8 0x05; u30 name]

  let of_ns_set =
    array ~f:(fun ns->[u30 ns])

  let of_multiname =
    function
	QName (ns,name) ->
	  [u8 0x07;u30 ns; u30 name]
      | QNameA (ns,name) ->
	  [u8 0x0D;u30 ns; u30 name]
      | RTQName name ->
	  [u8 0x0F; u30 name]
      | RTQNameA name ->
	  [u8 0x10; u30 name]
      | RTQNameL ->
	  [u8 0x11]
      | RTQNameLA ->
	  [u8 0x12]
      | Multiname (name,ns_set) ->
	  [u8 0x09; u30 name; u30 ns_set]
      | MultinameA (name,ns_set) ->
	  [u8 0x0E; u30 name; u30 ns_set]
      | MultinameL name ->
	  [u8 0x1B; u30 name]
      | MultinameLA name  ->
	  [u8 0x1C; u30 name]


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

  (* Trait *)
  let flags pats xs =
    let to_bit x =
      List.assoc x pats in
      List.fold_left (lor) 0 @@ List.map to_bit xs

  let of_trait_attrs =
    flags [ATTR_Final   ,0x01;
	   ATTR_Override,0x02]

  (* kind field contains two four-bit fields. The lower four bits determine the kind of this trait.
     The upper four bits comprise a bit vector providing attributes of the trait. *)
  let kind attr kind =
    u8 @@ ((of_trait_attrs attr) lsl 4) lor kind

  let of_trait_body =
    function
	SlotTrait (slot_id,type_name,vindex,vkind) ->
	  if vindex = 0 then
	    [u8 0;u30 slot_id; u30 type_name;u30 0]
	  else
	    [u8 0;u30 slot_id; u30 type_name;u30 vindex;u8 vkind]
      | MethodTrait (disp_id,meth,attrs) ->
	  [kind attrs 1;u30 disp_id; u30 meth]
      | GetterTrait (disp_id,meth,attrs) ->
	  [kind attrs 2;u30 disp_id; u30 meth]
      | SetterTrait (disp_id,meth,attrs) ->
	  [kind attrs 3;u30 disp_id; u30 meth]
      | ClassTrait  (slot_id,classi) ->
	  [u8 4; u30 slot_id; u30 classi]
      | FunctionTrait (slot_id,func) ->
	  [u8 5;u30 slot_id; u30 func]
      | ConstTrait (slot_id,type_name,vindex,vkind) ->
	  if vindex = 0 then
	    [u8 6;u30 slot_id; u30 type_name;u30 0]
	  else
	    [u8 6;u30 slot_id; u30 type_name;u30 vindex;u8 vkind]

  let of_trait {trait_name=name; data=data} =
    List.concat [[u30 name];
		 of_trait_body data]

  (* method *)
  let of_option_info xs =
    let detail value kind =
      [u30 value; u8 kind] in
    array xs ~f:begin function
	IntVal n ->
	  detail n 0x03
      | UIntVal n ->
	  detail n 0x04
      | DoubleVal n ->
	  detail n 0x06
      | StringVal n ->
	  detail n 0x01
      | BoolVal true ->
	  detail 0 0x0B
      | BoolVal false ->
	  detail 0 0x0A
      | NullVal ->
	  detail 0 0x0C
      | UndefinedVal ->
	  detail 0 0x00
      | NamespaceVal n ->
	  detail n 0x08
      | PackageNamespaceVal n ->
	  detail n 0x016
      | PackageInternalNamespaceVal n ->
	  detail n 0x17
      | ProtectedNamespaceVal n ->
	  detail n 0x18
      | ExplicitNamespaceVal n ->
	  detail n 0x19
      | StaticProtectedNamespaceVal n ->
	  detail n 0x1A
      | PrivateNamespaceVal n ->
	  detail n 0x15
    end

  let of_method_flags xs =
    let (flags, option, names) =
      StdLabels.List.fold_left xs ~init:(0,[],[]) ~f:begin fun (flags,option,names) x ->
      match x with
	  NeedArguments ->
	    (0x01 lor flags, option, names)
	| NeedActivation ->
	    (0x02 lor flags, option, names)
	| NeedRest ->
	    (0x04 lor flags, option, names)
	| SetDxns ->
	    (0x40 lor flags, option, names)
	| HasOptional xs ->
	    (0x08 lor flags, of_option_info xs, names)
	| HasParamNames xs ->
	    (0x80 lor flags, option, List.map u30 xs)
      end in
      List.concat [ [u8 flags];
		    option;
		    names]

  let of_method_info info =
    List.concat [[u30 (List.length info.params);
		  u30 info.return];
		 List.map u30 info.params;
		 [u30 info.method_name];
		 of_method_flags info.method_flags]

  let of_script {init=init; script_traits=traits} =
    (u30 init)::array ~f:of_trait traits

  let of_method_body body =
    let t =
      Label.make () in
      List.concat [
	[ u30 body.method_sig;
	  u30 body.max_stack;
	  u30 body.local_count;
	  u30 body.init_scope_depth;
	  u30 body.max_scope_depth];
	[backpatch 0 (fun addr map -> to_int_list [u30 (find map t - addr)])];
	HList.concat_map Inst.to_bytes body.code;
	[label t];
	dummy body.exceptions;
	array ~f:of_trait body.method_traits]

  let of_class  {cinit=init; class_traits=traits} =
    List.concat [
      [u30 init];
      array ~f:of_trait traits]

  let of_instance {instance_name = name;
		   super_name = sname;
		   instance_flags = flags;
		   interfaces = inf;
		   iinit = init;
		   instance_traits = traits} =
    let flag =
      function
	  Sealed        -> 0x01
	| Final         -> 0x02
	| Interface     -> 0x04
	| ProtectedNs _ -> 0x08 in
    let flags' =
      List.fold_left (fun x y -> x lor (flag y)) 0 flags in
    let ns =
      flags
      +> HList.concat_map begin function
	  ProtectedNs ns -> [u30 ns]
	| Sealed | Final | Interface -> []
      end
      +> function [] -> [] | x::_ -> [x] in
      List.concat [
	[u30 name;
	 u30 sname;
	 u8  flags'];
	ns;
	array ~f:(fun x -> [u30 x]) inf;
	[u30 init];
	array ~f:of_trait traits]

  let to_bytes { cpool;
		 method_info=info;
		 metadata;
		 classes;
		 instances;
		 scripts;
		 method_bodies=bodies; } =
    List.concat [
      [ u16 16; u16 46; ];
      of_cpool cpool;
      array ~f:of_method_info info;
      dummy metadata;
      array ~f:of_instance instances;
      HList.concat_map of_class classes;
      array ~f:of_script scripts;
      array ~f:of_method_body bodies
    ]
end
