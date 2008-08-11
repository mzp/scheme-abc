open Base
include Instruction

let make_meth ?(args=[]) name body = 
  let inst =
      body @
      [ReturnValue] in
  { name  = name;
    params= args;
    return=0;
    flags =0;
    exceptions=[];
    traits=[];
    instructions=inst}

let rec fold_method f init root =
  List.fold_left 
    (fun a subtree-> fold_method f a subtree)
    (f init root)
  @@ HList.concat_map (fun inst -> (get_config inst).meth) root.instructions

let fold_instruction f init =
  fold_method
    (fun init' {instructions=insts}->
       List.fold_left f init' insts)
    init

let collect_const =
  fold_instruction 
    (fun cpool i-> Cpool.append cpool (get_config i).const)
    Cpool.empty

let collect_klass =
  Set.to_list $ 
    fold_instruction (fun set i-> match (get_config i).klass with
			  Some k ->
			    Set.add k set
			| _ ->
			    set) Set.empty

let collect_method =
  Set.to_list $ fold_method (flip Set.add) Set.empty

(* convert instruction *)
let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let asm_method map index m =
  let configs =
    List.map get_config m.instructions in
  let zero =
    (0,0) in
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
      Abc.name=index; 
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

let add_flag flag ({Abc.flags_i=flags} as klass) =
  {klass with Abc.flags_i=flag::flags}

(*let asm_klass {cpool=cpool} =
  let add flag =
    Core.Tuple.T2.map2 ~f:(add_flag flag) in
  let rec loop =
    function
	Sealed k ->
	  add Abc.Sealed @@ loop k
      | Final k ->
	  add Abc.Final   @@ loop k
      | Interface k ->
	  add Abc.Interface @@ loop k
      | Protected (ns,k) ->
	  add (Abc.ProtectedNs (namespace_nget ns cpool)) @@ loop k
      | Class {cname=cname; sname=sname; interface=interface; methods=methods} ->
	  {Abc.=cname;Abc.trait_c=[]},failwith "" in
    loop*)
	

let assemble meth =
  let context =
    {cpool   = collect_const  meth;
     meths   = collect_method meth;
     klasses = collect_klass  meth} in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (asm_method context) context.meths in
    Cpool.to_abc context.cpool,info,body
