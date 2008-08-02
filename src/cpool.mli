type t

type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set


val empty : t
val append : t -> t -> t

val int:       int       -> t
val uint:      int       -> t
val double:    float     -> t
val string:    string    -> t
val namespace: namespace -> t
val multiname: multiname -> t

val to_abc : t -> Abc.cpool

val int_nget : int -> t -> int
val int_get : int -> t -> Bytes.t
val uint_nget : int -> t -> int
val uint_get : int -> t -> Bytes.t
val string_nget : string -> t -> int
val string_get : string -> t -> Bytes.t
val double_nget : float -> t -> int
val double_get : float -> t -> Bytes.t
val namespace_nget : namespace -> t -> int
val namespace_get : namespace -> t -> Bytes.t
val multiname_nget : multiname -> t -> int
val multiname_get : multiname -> t -> Bytes.t
