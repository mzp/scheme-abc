open Base
include Instruction

type t = {
  abc_cpool:     Abc.cpool;
  method_info:   Abc.method_info list;
  method_body:   Abc.method_body list;
  class_info:    Abc.class_info  list;
  instance_info: Abc.instance_info list
}


(* util function *)
let make_meth ?(args=[]) name body = 
  let inst =
    body @ [ReturnValue] in
    { name  = Cpool.make_qname name;
      params= args;
      return= 0;
      flags = 0;
      exceptions=[];
      traits= [];
      instructions=inst
    }

let make_proc ?(args=[]) name body = 
  let inst =
    body @ [ReturnVoid] in
    { name  = Cpool.make_qname name;
      params= args;
      return= 0;
      flags = 0;
      exceptions=[];
      traits= [];
      instructions=inst
    }

(**
   - meth contains instruction list.
   - instruction contains meth list.

   Thus, meth create multi-tree like structure.
   [fold_method f init meth] is folding this tree by [f].
*)
let rec fold_method f init meth =
  List.fold_left 
    (fun a inst -> List.fold_left (fold_method f) a (get_config inst).meth)
    (f init meth)
    meth.instructions

(** [fold_instruction f init meth] is recursively folding all-instruction by [f]. *)
let fold_instruction f init =
  fold_method
    (fun init' {instructions=insts}-> List.fold_left f init' insts)
    init

(**{6 Collecting some information}*)

(** [collect_const meth] returns all constant value which contained by [meth]. *)
let collect_const meth=
  Cpool.append
    (method_const meth)
    @@ fold_instruction 
    (fun cpool i-> Cpool.append cpool (get_config i).const)
    Cpool.empty meth


(** [collect_klass meth] returns all class which contained by [meth]. *)
let collect_klass =
  Set.to_list $ 
    fold_instruction (fun set i-> 
			match (get_config i).klass with
			  Some k ->
			    Set.add k set
			| _ ->
			    set) Set.empty

(** [collect_method meth] return all methods which contained by [meth]. *)
let collect_method =
  Set.to_list $ fold_method (flip Set.add) Set.empty

(** {6 Assemble meth} *)

let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let asm_method map index m =
  let zero =
    (0,0) in
  let configs =
    List.map get_config m.instructions in
  let (max_stack,_),(max_scope,_),local_count,bytes = 
    List.fold_left
      (fun 
	 (stack,scope,count,bytes) 
	 {op=op;prefix=prefix;args=args;stack=st;scope=sc;count=c} -> 
	   let by =
	     List.concat [
	       prefix map;
	       [Bytes.u8 op];
	       args map] in
	     add stack st,add scope sc,max count c,by::bytes)
      (zero,zero,1,[]) configs in
  let info =
    { Abc.params=m.params; 
      Abc.return=m.return; 
      Abc.name=Cpool.multiname_nget m.name map.cpool;
      Abc.flags=m.flags } in
  let body =
    { Abc.method_sig=index;
      Abc.max_stack=max_stack;
      Abc.local_count=List.length m.params+1;
      Abc.init_scope_depth=0; 
      Abc.max_scope_depth=max_scope;
      Abc.code=List.concat @@ List.rev bytes;
      Abc.exceptions=[]; 
      Abc.trait_m=[] } in
      info,body

let asm_klass {cpool=cpool; meths=meths; klasses=klasses} klass =
  let class_info = {
    Abc.cinit   = index klass.cinit meths;
    Abc.trait_c = []; 
  } in
  let flag_conv = function
      Sealed -> Abc.Sealed 
    | Final  -> Abc.Final 
    | Interface -> Abc.Interface
    | ProtectedNs ns -> Abc.ProtectedNs (Cpool.namespace_nget ns cpool) in
  let method_conv m = {
    Abc.t_name = Cpool.multiname_nget m.name cpool;
    data       = Abc.MethodTrait (0,index m meths)
  } in
  let instance_info = {
    Abc.name_i = Cpool.multiname_nget klass.cname cpool;
    super_name = Cpool.multiname_nget klass.sname cpool;
    flags_i    = List.map flag_conv klass.flags_k;
    interface  = List.map (flip index klasses) klass.interface;
    iinit      = index klass.iinit meths;
    trait_i    = List.map method_conv klass.methods;
  } in
    class_info,instance_info
	

let assemble meth =
  let context =
    {cpool   = collect_const  meth;
     meths   = collect_method meth;
     klasses = collect_klass  meth} in
  let info,body =
    List.split @@ ExtList.List.mapi (asm_method context) context.meths in
  let class_info,instance_info =
    List.split @@ List.map (asm_klass context) context.klasses in
    {abc_cpool     = Cpool.to_abc context.cpool;
     method_info   = info;
     method_body   = body;
     class_info    = class_info;
     instance_info = instance_info}
