open Base
open Bytes

(* data flow *)
let fork2 f g x       = (f x, g x)
let fork3 f g h x     = (f x, g x, h x)
let fork4 f g h i x   = (f x, g x, h x, i x)

let with2 f g (a,b) = (f a, g b)
let with3 f g h (a,b,c) = (f a, g b, h c)
let with4 f g h i (a,b,c,d) = (f a, g b, h c, i d)

let join2 f (a,b)     = f a b
let join3 f (a,b,c)   = f a b c
let join4 f (a,b,c,d) = f a b c d

module type Spec = sig
  type t
  val spec : t -> t ISpec.t
end

type t = {
  cpool:         Cpool.t;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}

module Make(Spec:Spec) = struct
  (* type *)
  type method_ = Spec.t ISpec.method_
  type class_  = Spec.t ISpec.class_
  type context = Spec.t ISpec.context
  type instruction = Spec.t

  (* fold *)
  type ghost = [
    `Script         of method_
  | `InstanceMethod of method_
  | `StaticMethod   of method_
  | `InstanceInit   of method_
  | `ClassInit      of method_
  ]

  (* help me :
     I want to write:
       type inst [ ghost | Spec.t]

     But compiler says: "Spec.t is not poly variants"
  *)
  type inst = [
    ghost
  | `Inst  of Spec.t ]

  let method_ : inst -> method_ option =
    function
	`InstanceMethod m
      | `StaticMethod m
      | `Script m
      | `InstanceInit m
      | `ClassInit m ->
	  Some m
      | `Inst inst ->
	  ((Spec.spec inst).ISpec.method_)

  let class_ : inst -> class_ option =
    function
	`InstanceMethod _ | `StaticMethod _ | `Script _ | `InstanceInit _ | `ClassInit _ ->
	  None
      | `Inst inst ->
	  ((Spec.spec inst).ISpec.class_)

  let fold f init inst =
    let rec loop ctx inst =
      let method_ctx =
	match method_ inst with
	    Some {ISpec.instructions=instructions} ->
	      let instructions' =
		instructions
		+>  List.map (fun i -> `Inst i) in
	      let ctx' =
		List.fold_left loop (ctx#current_method <- init#current_method)
		  (instructions' :> inst list) in
		(ctx'#sub_method <- ctx'#current_method)#current_method <-
		  ctx#current_method
	  | None ->
	      ctx in
      let class_ctx =
	match class_ inst with
	    Some { ISpec.iinit=iinit;
		   cinit=cinit;
		   instance_methods = im;
		   static_methods = sm } ->
	      let ctx' =
		loop method_ctx (`InstanceInit iinit) in
	      let ctx'' =
		loop ctx' (`ClassInit cinit) in
	      let ctx''' =
		List.fold_left (fun ctx m -> loop ctx (`InstanceMethod m)) ctx'' im in
		List.fold_left (fun ctx m -> loop ctx (`StaticMethod m)) ctx''' sm
	  | None ->
	      method_ctx in
	f class_ctx inst in
      loop init inst

  (* dataflow block *)
  let filter_const inst =
    let inst_const =
      match inst with
	  #ghost ->
	    []
	| `Inst i  ->
	    (Spec.spec i).ISpec.const in
    let method_const =
      match method_ inst with
	  Some {ISpec.method_name = name } ->
	    [name]
	| None ->
	    [] in
    let class_const  =
      match class_ inst with
	  Some {ISpec.class_name=class_name; super=super; attributes=attributes} ->
	    class_name::super::attributes
	| None ->
	    [] in
      inst_const @ (method_const :> Cpool.entry list) @ (class_const :> Cpool.entry list)

  let filter_class =
    function
	#ghost ->
	  None
      | `Inst inst ->
	  ((Spec.spec inst).ISpec.class_) (* extra paren is inserted for tuarge-mode *)

  let filter_method =
    (method_)  (* extra paren is inserted for tuarge-mode *)

  let if_some f init =
    function
	Some x ->
	  f init x
      | None ->
	  init

  let make_context ctx const (class_ : class_ option) (method_ : method_ option) =
    let ctx =
      ctx#cpool <- List.fold_left (flip Cpool.add) ctx#cpool const in
    let ctx =
      if_some (fun ctx c -> ctx#classes <- RevList.add c ctx#classes ) ctx class_ in
    let ctx =
      if_some (fun ctx m -> ctx#methods <- RevList.add m ctx#methods ) ctx method_ in
      ctx

  (* make *)
  let make_inst ctx =
    function
	#ghost ->
	  None
      | `Inst inst ->
	  let {ISpec.op=op; prefix=prefix; args=args} =
	    Spec.spec inst in
	    Some (List.concat [
		    prefix (ctx :> context);
		    [u8 op];
		    args   (ctx :> context)])

  let make_class ~cpool ~classes ~methods inst =
    let make c =
      let flag =
	function
	    `Sealed         -> Abc.Sealed
	  | `Final          -> Abc.Final
	  | `Interface      -> Abc.Interface
	  | `ProtectedNs ns -> Abc.ProtectedNs (Cpool.index ns cpool) in
      let method_attr =
	function `Override -> Abc.ATTR_Override
	  |      `Final    -> Abc.ATTR_Final in
      let method_trait m = {
	Abc.trait_name = Cpool.index m.ISpec.method_name cpool;
	data           = Abc.MethodTrait (0,
					  RevList.index m methods,
					  List.map method_attr m.ISpec.method_attrs) } in
      let attr_trait id attr = {
	Abc.trait_name = Cpool.index attr cpool;
	data       = Abc.SlotTrait (id+1,0,0,0) } in
      let class_info = {
	Abc.cinit    = RevList.index c.ISpec.cinit methods;
	class_traits = List.map method_trait c.ISpec.static_methods
      } in
      let instance_info = {
	Abc.instance_name =
	  Cpool.index c.ISpec.class_name cpool;
	super_name        =
	  Cpool.index c.ISpec.super cpool;
	instance_flags    =
	  List.map flag c.ISpec.class_flags;
	interface         =
	  List.map (flip RevList.index classes) c.ISpec.interface;
	iinit             =
	  RevList.index c.ISpec.iinit methods;
	instance_traits   =
	  List.concat [
	    List.map method_trait c.ISpec.instance_methods;
	    ExtList.List.mapi attr_trait c.ISpec.attributes
	  ]
      } in
	class_info,instance_info in
      sure make @@  class_ inst

  (* make method *)
  let empty_usage = object
    val stack = (0,0) with accessor
    val scope = (0,0) with accessor
  end

  let add_usage i (current,max_value)=
    (current + i, max max_value (current+i))
  let filter_usage usage =
    function
	#ghost ->
	usage
      | `Inst inst ->
	  let {ISpec.stack=stack; scope=scope} =
	    Spec.spec inst in
	  let usage =
	    usage#stack <- add_usage stack usage#stack in
	  let usage =
	    usage#scope <- add_usage scope usage#scope in
	    usage

  let mn_name =
    function
	`QName (_,str) ->
	  str
      | `Multiname (str,_) ->
	  str

  let make_method ~cpool ~insts ~usage inst =
    let make m =
      let info =
	{ Abc.params   = m.ISpec.params;
	  return       = m.ISpec.return;
	  method_name  = Cpool.index (`String (mn_name m.ISpec.method_name)) cpool;
	  method_flags = m.ISpec.method_flags } in
      let body =
	{ Abc.method_sig   = -1; (* dummy *)
	  max_stack        = snd usage#stack;
	  local_count      = List.length m.ISpec.params+1;
	  init_scope_depth = 0;
	  max_scope_depth  = snd usage#scope;
	  code             = List.concat @@ List.rev insts;
	  exceptions       = [];
	  method_traits    = [] } in
	info,body in
      sure make @@ method_ inst

  let ($>) g f x = f (g x)

  (* pipeline *)
  let pipeline (ctx :'a) inst : 'a =
    inst
    +> fork2
      (fork2
	 (fork3 filter_const filter_class filter_method  $> join3 (make_context ctx))
	 id
	 $> fork4
	 fst
	 (curry make_inst)
	 (fun (ctx,inst) ->
	    make_class
	      ~cpool:ctx#cpool
	      ~classes:ctx#classes
	      ~methods:ctx#methods
	      inst)
	 (fun (ctx,inst) ->
	    make_method
	      ~cpool:ctx#cpool
	      ~insts:ctx#sub_method#insts
	      ~usage:ctx#sub_method#usage
	      inst))
      (filter_usage ctx#current_method#usage)
    +> (fun ((ctx, inst, c, m), usage) ->
	  let current_method =
	    if_some (fun c i -> c#insts <- i::c#insts) ctx#current_method inst in
	  let current_method =
	    current_method#usage <- usage in
	  let ctx =
	    ctx#current_method <- current_method in
	  let ctx =
	    if_some (fun c m -> c#abc_methods <- m::c#abc_methods) ctx m in
	  let ctx =
	    if_some (fun c m -> c#abc_classes <- m::c#abc_classes) ctx c in
	    ctx)

  let context = object
    val cpool = Cpool.empty with accessor
    val abc_methods = [] with accessor
    val abc_classes = [] with accessor

    val methods = RevList.empty with accessor
    val classes = RevList.empty with accessor

    val current_method = object
      val insts = [] with accessor
      val usage = empty_usage with accessor
    end with accessor

    val sub_method = object
      val insts = [] with accessor
      val usage = empty_usage with accessor
    end with accessor
  end

  let assemble_slot_traits cpool xs =
    xs
    +> List.map (fun (name,id)-> {
		   Abc.trait_name = Cpool.index name cpool;
		   data           = Abc.SlotTrait (id,0,0,0);
		 })

  let assemble_method m =
    let ctx =
      fold pipeline context (`Script m) in
      {
	cpool         = ctx#cpool;
	method_info   = List.rev_map fst ctx#abc_methods;
	method_body   = ctx#abc_methods
          +> List.rev_map snd
          +> ExtList.List.mapi (fun i m -> {m with Abc.method_sig=i});
	class_info    = List.rev_map fst ctx#abc_classes;
	instance_info = List.rev_map snd ctx#abc_classes;
      }

  let assemble slots m =
    let { cpool         = cpool;
	  method_info   = info;
	  method_body   = body;
	  class_info    = class_info;
	  instance_info = instance_info} =
      assemble_method m in
    let cpool,slots' =
      map_accum_left
	(fun cpool ((ns,name),i)->
	   let qname =
	     `QName(`Namespace (String.concat "." ns), name) in
	       (Cpool.add qname cpool,(qname,i)))
	cpool
	slots in
    let slot_traits =
      assemble_slot_traits cpool slots' in
    let class_traits =
      let n =
	List.length slots in
	ExtList.List.mapi
	  (fun i {Abc.instance_name=name} ->
	     {Abc.trait_name=name; data=Abc.ClassTrait (i+n+1,i)})
	  instance_info in
      { Abc.cpool   = Cpool.to_abc cpool;
	method_info = info;
	method_bodies = body;
	metadata    = [];
	classes     = class_info;
	instances   = instance_info;
	scripts     = [{
			 Abc.init = List.length info - 1;
			 script_traits =  slot_traits @ class_traits
		       }]}
end
