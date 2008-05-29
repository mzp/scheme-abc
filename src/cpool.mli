type t
type cmap

type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set


val empty : t
val append : t -> t -> t
val int: int -> t
val uint: int -> t
val string: string -> t
val multiname: multiname -> t

val int_get       : int -> cmap -> Bytes.t
val uint_get      : int -> cmap -> Bytes.t
val string_get    : string -> cmap -> Bytes.t
val string_nget    : string -> cmap -> int
val multiname_get : multiname -> cmap -> Bytes.t
val multiname_nget : multiname -> cmap -> int

val to_cmap  : t -> cmap
val to_cpool : cmap -> Abc.cpool
