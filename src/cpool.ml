open Base
type t = Abc.cpool
type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

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
  namespace: (namespace*Abc.namespace) map;
  namespace_set: (namespace_set*Abc.namespace_set) map;
  multiname: (multiname*Abc.multiname) map;
}

let ns_name = 
  function Namespace name | PackageNamespace name ->
    name

(* clist *)
let append lhs rhs = 
  {lint           = lhs.lint @ rhs.lint;
   luint          = lhs.luint @ rhs.luint;
   ldouble        = lhs.ldouble @ rhs.ldouble;
   lstring        = lhs.lstring @ rhs.lstring;
   lnamespace     = lhs.lnamespace @ rhs.lnamespace ;
   lnamespace_set = lhs.lnamespace_set @ rhs.lnamespace_set;
   lmultiname     = lhs.lmultiname @ rhs.lmultiname }

let empty = 
  {lint=[]; 
   luint=[]; 
   ldouble=[]; 
   lstring=[]; 
   lnamespace=[]; 
   lnamespace_set=[]; 
   lmultiname=[]}

let int x = {
  empty with lint=[x]
}

let uint x = {
  empty with luint=[x]
}

let string x = {
  empty with lstring=[x]
}

let multiname name= 
  match name with
      QName (ns,str) ->
	{empty with 
	   lnamespace=[ns];
	   lstring=[ns_name ns;str];
	   lmultiname=[name] }
    | Multiname (str,ns_set) ->
	{empty with
	   lnamespace_set=[ns_set];
	   lnamespace=ns_set;
	   lstring=str::List.map ns_name ns_set;
	   lmultiname=[name] }

(* cmap *)
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

let string_get str {string=smap} = 
  Bytes.u30 (base_get str smap)

let int_get n {int=imap} = 
  Bytes.u30 (base_get n imap)

let uint_get n {uint=umap} = 
  Bytes.u30 (base_get n umap)

let multiname_get name {multiname=nmap} =
  Bytes.u30 (complex_get name nmap)

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

let cpool_of_cmap {int=int;
		   uint=uint;
		   double=double;
		   string=string;
		   namespace=ns;
		   namespace_set=ns_set;
		   multiname=name}= 
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
