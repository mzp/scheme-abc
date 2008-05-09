open Base

type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

type instruction = 
    GetLocal 
  | PushScope 
  | ReturnVoid 
  | FindPropStrict of multiname 
  | PushString of string 
  | PushInt of int
  | CallPropLex of multiname * int
  | Pop

(* constant list *)
type clist = {
  lint:    int list;
  luint:   int list;
  ldouble: float list;
  lstring: string list;
  lnamespace: namespace list;
  lnamespace_set: namespace_set list;
  lmultiname: multiname list;
}

type 'a map = ('a*int) list
type cmap = {
  int:    int map;
  uint:   int map;
  double: float map;
  string: string map;
  namespace:     (namespace*Abc.namespace) map;
  namespace_set: (namespace_set*Abc.namespace_set) map;
  multiname:     (multiname*Abc.multiname) map;
}

(* code *)
type code = { bytes:Bytes.t list;
	      stack:int;
	      scope:int;
	      count:int}

type meth = {
  name: string;
  params: int list;
  return: int;
  flags:int;
  instructions:instruction list;
  traits: int list;
  exceptions: int list;
}

let empty_clist = 
  {lint=[]; 
   luint=[]; 
   ldouble=[]; 
   lstring=[]; 
   lnamespace=[]; 
   lnamespace_set=[]; 
   lmultiname=[]}


let append_clist lhs rhs = 
  {lint           = lhs.lint @ rhs.lint;
   luint          = lhs.luint @ rhs.luint;
   ldouble        = lhs.ldouble @ rhs.ldouble;
   lstring        = lhs.lstring @ rhs.lstring;
   lnamespace     = lhs.lnamespace @ rhs.lnamespace ;
   lnamespace_set = lhs.lnamespace_set @ rhs.lnamespace_set;
   lmultiname     = lhs.lmultiname @ rhs.lmultiname }

let ns_name = 
  function Namespace name | PackageNamespace name ->
    name
    
let collect xs =
  let f ({lstring=lstring;
	  lint=lint;
	  lnamespace=lns;
	  lnamespace_set=lns_set;
	  lmultiname=lname} as clist) = 
    function
	PushString str ->
	  {clist with lstring=str::lstring}
      | PushInt n ->
	  {clist with lint=n::lint}
      | FindPropStrict name | CallPropLex (name,_) ->
	  begin match name with
	      QName (ns,str) ->
		{clist with 
		   lnamespace=ns::lns;
		   lstring=(ns_name ns)::str::lstring;
		   lmultiname=name::lname}
	    | Multiname (string,ns_set) ->
		{clist with
		   lnamespace_set=ns_set::lns_set;
		   lnamespace=(ns_set)@lns;
		   lstring=(List.map ns_name ns_set)@lstring;
		   lmultiname=name::lname } end
      | _ -> 
	  clist in
    List.fold_left  f empty_clist xs

let base_get =
  List.assoc

let complex_get x xs =
  snd @@ List.find (fun ((y,_),i)-> x = y) xs

let of_namespace map =
  function 
      Namespace name ->
	{Abc.kind=0x08;Abc.ns_name=base_get name map}
    | PackageNamespace name ->
	{Abc.kind=0x16;Abc.ns_name=base_get name map}

let of_multiname smap nsmap nssmap =
  function 
      QName (ns,s) ->
	Abc.QName (complex_get ns nsmap,base_get s smap)
    | Multiname (s,nss) ->
	Abc.Multiname (base_get s smap,complex_get nss nssmap)

let cmap_of_clist {lint=int; 
		   luint=uint;
		   ldouble=double;
		   lstring=string;
		   lnamespace=ns;
		   lnamespace_set=ns_set;
		   lmultiname=name} =
  let simple_pack l
      = ExtList.List.mapi (fun i x->(x,i+1)) @@ ExtList.List.unique @@ List.sort compare l in
  let int' = 
    simple_pack int in
  let uint' =
    simple_pack uint in
  let double' =
    simple_pack double in
  let string' =
    simple_pack string in
  let ns' =
    List.map (fun (n,i) -> ((n,of_namespace string' n),i)) @@ simple_pack ns in
  let ns_set' =
    let abc n = 
      List.map (flip complex_get ns') n in
      List.map (fun (n,i) -> ((n,(abc n)),i)) @@ simple_pack ns_set in
  let name' =
    List.map (fun (n,i) -> ((n,of_multiname string' ns' ns_set' n),i)) @@ simple_pack name in
    {int=int'; 
     uint=uint'; 
     double=double';
     string=string';
     namespace=ns';
     namespace_set=ns_set';
     multiname=name'}

let cpool_of_cmap {int=int;uint=uint;double=double;string=string;namespace=ns;namespace_set=ns_set;multiname=name}= 
  let base xs = 
    List.map fst xs in
  let complex xs =
    List.map (snd$fst) xs in
  {
    Abc.int = base int;
    Abc.uint = base uint;
    Abc.double=base double;
    Abc.string=base string;
    Abc.namespace=complex ns;
    Abc.namespace_set=complex ns_set;
    Abc.multiname=complex name
  }

let add (max,current) n = 
  let current' =
    current + n in
    if max < current' then
      (current',current')
    else
      (max,current')

let inc s = add s 1
let dec s = add s ~-1

let str_get str {string=smap} = 
  Bytes.U30 (base_get str smap)

let int_get n {int=imap} = 
  Bytes.U30 (base_get n imap)

let name_get name {multiname=nmap} =
  Bytes.U30 (complex_get name nmap)

let empty_code = {
  bytes=[];
  stack=0;
  scope=0;
  count=0 }

(* convert instruction *)
let encode cmap =
  function 
      GetLocal -> 
	{empty_code with bytes=[Bytes.U8 0xD0]; stack=1}
    | PushScope -> 
	{empty_code with bytes=[Bytes.U8 0x30]; stack=(-1); scope=1}
    | ReturnVoid -> 
	{empty_code with bytes=[Bytes.U8 0x47]}
    | PushString str ->
	{empty_code with bytes=[Bytes.U8 0x2C;str_get str cmap]; stack=1}
    | PushInt n ->
	{empty_code with bytes=[Bytes.U8 0x2D;int_get n cmap]; stack=1}
    | FindPropStrict name ->
	{empty_code with bytes=[Bytes.U8 0x5D;name_get name cmap]; stack=1}
    | Pop ->
	{empty_code with bytes=[Bytes.U8 0x29]; stack=(-1)}
    | CallPropLex(name,count) ->
	{empty_code with 
	   bytes=[Bytes.U8 0x4C;name_get name cmap;Bytes.U30 count]; 
	   stack=(-count)}

let method_asm cmap index m =
  let codes =
    List.map (encode cmap) m.instructions in
  let (max_stack,_),(max_scope,_),local_count,bytes = 
    List.fold_left 
      (fun 
	 (stack,scope,count,bytes) 
	 {stack=st;scope=sc;count=c;bytes=by} -> 
	   (add stack st,add scope sc,count+c,(List.rev by)@bytes))
      ((0,0),(0,0),1,[])
      codes in
  let info =
    { Abc.params=m.params; 
      Abc.return=m.return; 
      Abc.name=index; 
      Abc.flags=m.flags } in
  let body =
    { Abc.method_sig=index;
      Abc.max_stack=max_stack;
      Abc.local_count=local_count;
      Abc.init_scope_depth=0; 
      Abc.max_scope_depth=max_scope;
      Abc.code=List.rev bytes;
      Abc.exceptions=[]; 
      Abc.trait_m=[] } in
      info,body

let assemble methods =
  let cmap = 
    cmap_of_clist
    @@ List.fold_left append_clist empty_clist 
    @@ List.map (fun {instructions=inst}->collect inst) methods in
  let cpool =
    cpool_of_cmap cmap in
  let info,body =
    ExtList.List.split @@ ExtList.List.mapi (fun i x-> method_asm cmap i x) methods in
    cpool,info,body

let test () = 
  let m = 
    { name  ="main";
      params=[];
      return=0;
      flags =0;
      exceptions=[];
      traits=[];
      instructions=[GetLocal;
		    PushScope;
		    FindPropStrict (QName ((Namespace ""),"print"));
		    PushInt 42;
		    CallPropLex (QName ((Namespace ""),"print"),1);
		    Pop;
		    ReturnVoid]} in
  let cpool,info,body = 
    assemble [m] in
  let script  =
    { Abc.init=0; Abc.trait_s=[]} in
  let abc =
    {Abc.cpool=cpool; 
     Abc.method_info=info;
     Abc.method_body=body;
     Abc.metadata=[]; Abc.classes=[]; Abc.instances=[]; 
     Abc.script=[script]; } in
  let ch = open_out_bin "asm.abc" in
    Bytes.output_bytes ch @@ Abc.bytes_of_abc abc;
    close_out ch;
    abc
