open Base
type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

type ('a,'b,'c,'d,'e,'f,'g) constants = {
  int: 'a;
  uint: 'b;
  double: 'c;
  string: 'd;
  namespace: 'e;
  namespace_set: 'f;
  multiname: 'g;
}
type map = (int Pool.map,
	    int Pool.map,
	    float Pool.map,
	    string Pool.map,
	    namespace Pool.map,
	    namespace_set Pool.map,
	    multiname Pool.map) constants
type table = (int Pool.t,
	      int Pool.t,
	      float Pool.t,
	      string Pool.t,
	      namespace Pool.t,
	      namespace_set Pool.t,
	      multiname Pool.t) constants
type t = table
type cmap = table * map

(* for table *)
type op = {app: 'a . 'a Pool.t -> 'a Pool.t -> 'a Pool.t}

let lift2 {app=f} x y =
  {int           = f x.int           y.int;
   uint          = f x.uint          y.uint;
   double        = f x.double        y.double;
   string        = f x.string        y.string;
   namespace     = f x.namespace     y.namespace;
   namespace_set = f x.namespace_set y.namespace_set;
   multiname     = f x.multiname     y.multiname}

let empty = 
  {int           = Pool.empty;
   uint          = Pool.empty;
   double        = Pool.empty; 
   string        = Pool.empty;
   namespace     = Pool.empty; 
   namespace_set = Pool.empty;
   multiname     = Pool.empty}

let lift1 f x =
  lift2 f x empty

let append x y = 
  lift2 {app=Pool.append} x y

let ns_name = 
  function Namespace name | PackageNamespace name ->
    name

let make x =
  Pool.add x Pool.empty

let int x = {
  empty with int=make x
}

let uint x = {
  empty with uint=make x
}

let string x = {
  empty with string=make x
}

let double x = {
  empty with double=make x
}

let namespace x = {
  empty with 
    namespace=make x;
    string=make @@ ns_name x
}

let multiname name= 
  match name with
      QName (ns,str) ->
	{empty with 
	   namespace=make ns;
	   string=Pool.of_list [ns_name ns;str];
	   multiname=make name }
    | Multiname (str,ns_set) ->
	{empty with
	   namespace_set=make ns_set;
	   namespace=Pool.of_list ns_set;
	   string=Pool.of_list (str::List.map ns_name ns_set);
	   multiname=make name }

(* for cmap *)
let pool_get v tbl =
  1+Pool.get v tbl

(* for cpool *)
let cpool_entry v map =
  Bytes.u30 (pool_get v map)

let string_get str (_,{string=map}) = 
  cpool_entry str map

let string_nget str (_,{string=map}) = 
  pool_get str map

let int_get n (_,{int=map}) = 
  cpool_entry n map

let uint_get n (_,{uint=map}) = 
  cpool_entry n map

let double_get v (_,{double=map}) = 
  cpool_entry v map

let namespace_get n (_,{namespace=map}) =
  cpool_entry n map

let uint_get n (_,{uint=map}) = 
  cpool_entry n map

let multiname_get name (_,{multiname=map}) =
  cpool_entry name map

let multiname_nget name (_,{multiname=map}) =
  pool_get name map

(* conversion *)
let pack x =
  lift1 {app=fun x _ -> Pool.uniq x} x;;

let to_map x =
  {int           = Pool.to_map x.int;
   uint          = Pool.to_map x.uint;
   double        = Pool.to_map x.double;
   string        = Pool.to_map x.string;
   namespace     = Pool.to_map x.namespace;
   namespace_set = Pool.to_map x.namespace_set;
   multiname     = Pool.to_map x.multiname}

let to_cmap tbl =
  let tbl' =
    pack tbl in
    tbl',to_map tbl'

(* for abc *)
let of_namespace {string=map} =
  function 
      Namespace name ->
	{Abc.kind=0x08;Abc.ns_name=pool_get name map}
    | PackageNamespace name ->
	{Abc.kind=0x16;Abc.ns_name=pool_get name map}

let of_namespace_set {namespace=map} nss =
  List.map (fun ns -> pool_get ns map) nss

let of_multiname {string=smap; namespace=nsmap; namespace_set=nssmap} =
  function 
      QName (ns,s) ->
	Abc.QName (pool_get ns nsmap,pool_get s smap)
    | Multiname (s,nss) ->
	Abc.Multiname (pool_get s smap,pool_get nss nssmap)

let to_cpool (tbl,map) =
  {
    Abc.int   = Pool.to_list tbl.int;
    Abc.uint  = Pool.to_list tbl.uint;
    Abc.double= Pool.to_list tbl.double;
    Abc.string= Pool.to_list tbl.string;
    Abc.namespace=List.map (of_namespace map) @@ Pool.to_list tbl.namespace;
    Abc.namespace_set=List.map (of_namespace_set map) @@ Pool.to_list tbl.namespace_set;
    Abc.multiname=List.map (of_multiname map) @@ Pool.to_list tbl.multiname
  }
