open Base

module type Inst = sig
  type t
  val to_bytes : t -> BytesOut.t list
end

module Make(Inst : Inst) = struct
  open BytesOut
  open AbcType

  let dummy _ = [u30 0]

  let array f xs =
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
    array (fun c -> [u8 (Char.code c)]) @@ ExtString.String.explode str

  let of_ns {kind=kind;namespace_name=name} =
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

  (* Trait *)
  let of_trait_attrs attrs =
    let of_attr attr = List.assoc attr [ATTR_Final   ,0x01;
					ATTR_Override,0x02;
					ATTR_Medadata,0x04] in
      List.fold_left (lor) 0 @@ List.map of_attr attrs

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

  (* ----------------------------------------
     Other
     ---------------------------------------- *)
  let of_method_info info =
    List.concat [[u30 (List.length info.params);
		  u30 info.return];
		 List.map u30 info.params;
		 [u30 info.method_name;
		  u8  info.method_flags]]

  let of_script {init=init; script_traits=traits} =
    (u30 init)::array of_trait traits

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
	array of_trait body.method_traits]

  let of_class  {cinit=init; class_traits=traits} =
    List.concat [
      [u30 init];
      array of_trait traits]

  let of_instance {instance_name = name;
		   super_name = sname;
		   instance_flags = flags;
		   interface = inf;
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
	array (fun x -> [u30 x]) inf;
	[u30 init];
	array of_trait traits]

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
      array of_method_info info;
      dummy metadata;
      array of_instance instances;
      HList.concat_map of_class classes;
      array of_script scripts;
      array of_method_body bodies
    ]
end
