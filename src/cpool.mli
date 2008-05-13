type t = Abc.cpool
type clist
type cmap

type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

val empty : clist
val append : clist -> clist -> clist
val int: int -> clist
val uint: int -> clist
val string: string -> clist
val multiname: multiname->clist

val int_get : int -> cmap -> Bytes.t
val uint_get : int -> cmap -> Bytes.t
val string_get : string -> cmap -> Bytes.t
val multiname_get :multiname -> cmap -> Bytes.t

val cmap_of_clist : clist->cmap
val cpool_of_cmap : cmap->t

