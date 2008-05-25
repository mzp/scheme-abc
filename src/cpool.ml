open Base
type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

type table = {
  lint:    int Table.t;
  luint:   int Table.t;
  ldouble: float Table.t;
  lstring: string Table.t;
  lnamespace: namespace Table.t;
  lnamespace_set: namespace_set Table.t;
  lmultiname: multiname Table.t;
}
type t = table

type map = {
  int:    int Table.map;
  uint:   int Table.map;
  double: float Table.map;
  string: string Table.map;
  namespace: namespace Table.map;
  namespace_set: namespace_set Table.map;
  multiname: multiname Table.map;
}
type cmap = table * map

let ns_name = 
  function Namespace name | PackageNamespace name ->
    name

(* clist *)
let append lhs rhs = 
  {lint           = Table.append lhs.lint rhs.lint;
   luint          = Table.append lhs.luint rhs.luint;
   ldouble        = Table.append lhs.ldouble rhs.ldouble;
   lstring        = Table.append lhs.lstring rhs.lstring;
   lnamespace     = Table.append lhs.lnamespace rhs.lnamespace ;
   lnamespace_set = Table.append lhs.lnamespace_set rhs.lnamespace_set;
   lmultiname     = Table.append lhs.lmultiname rhs.lmultiname }

let empty = 
  {lint=Table.empty;
   luint=Table.empty;
   ldouble=Table.empty; 
   lstring=Table.empty;
   lnamespace=Table.empty; 
   lnamespace_set=Table.empty;
   lmultiname=Table.empty}

let make x =
  Table.add x Table.empty

let int x = {
  empty with lint=make x
}

let uint x = {
  empty with luint=make x
}

let string x = {
  empty with lstring=make x
}

let multiname name= 
  match name with
      QName (ns,str) ->
	{empty with 
	   lnamespace=make ns;
	   lstring=Table.of_list [ns_name ns;str];
	   lmultiname=make name }
    | Multiname (str,ns_set) ->
	{empty with
	   lnamespace_set=make ns_set;
	   lnamespace=Table.of_list ns_set;
	   lstring=Table.of_list (str::List.map ns_name ns_set);
	   lmultiname=make name }

(* cmap *)
let table_get v tbl =
  1+Table.get v tbl

let of_namespace map =
  function 
      Namespace name ->
	{Abc.kind=0x08;Abc.ns_name=table_get name map}
    | PackageNamespace name ->
	{Abc.kind=0x16;Abc.ns_name=table_get name map}

let of_multiname smap nsmap nssmap =
  function 
      QName (ns,s) ->
	Abc.QName (table_get ns nsmap,table_get s smap)
    | Multiname (s,nss) ->
	Abc.Multiname (table_get s smap,table_get nss nssmap)

let string_get str (_,{string=smap}) = 
  Bytes.u30 (table_get str smap)

let int_get n (_,{int=imap}) = 
  Bytes.u30 (table_get n imap)

let uint_get n (_,{uint=umap}) = 
  Bytes.u30 (table_get n umap)

let multiname_get name (_,{multiname=nmap}) =
  Bytes.u30 (table_get name nmap)

let pack x =
  {lint           = Table.uniq x.lint;
   luint          = Table.uniq x.luint;
   ldouble        = Table.uniq x.ldouble;
   lstring        = Table.uniq x.lstring;
   lnamespace     = Table.uniq x.lnamespace;
   lnamespace_set = Table.uniq x.lnamespace_set;
   lmultiname     = Table.uniq x.lmultiname;}

let to_cmap tbl =
  let tbl' =
    pack tbl in
    tbl',{int=Table.to_map tbl'.lint;
	  uint=Table.to_map tbl'.luint; 
	  double=Table.to_map tbl'.ldouble;
	  string=Table.to_map tbl'.lstring;
	  namespace=Table.to_map tbl'.lnamespace;
	  namespace_set=Table.to_map tbl'.lnamespace_set;
	  multiname=Table.to_map tbl'.lmultiname}

let to_cpool (tbl,map) =
  {
    Abc.int   = Table.to_list tbl.lint;
    Abc.uint  = Table.to_list tbl.luint;
    Abc.double= Table.to_list tbl.ldouble;
    Abc.string= Table.to_list tbl.lstring;
    Abc.namespace=List.map (fun x->of_namespace map.string x) @@ 
      Table.to_list tbl.lnamespace;
    Abc.namespace_set=List.map (fun x->List.map (fun y -> table_get y map.namespace) x) @@ Table.to_list tbl.lnamespace_set;
    Abc.multiname=List.map (fun x->of_multiname map.string map.namespace map.namespace_set x) @@ 
      Table.to_list tbl.lmultiname
  }
