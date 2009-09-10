(**
    Constant pool(CPool).

    CPool create the map from a value to the index, or a list of the value.
 *)

(** A type of namespace *)
type namespace = [
  `Namespace of string
| `PackageNamespace of string
| `PackageInternalNamespace of string
| `ProtectedNamespace of string
| `ExplicitNamespace of string
| `StaticProtectedNamespace of string
| `PrivateNamespace of string ]

type namespace_set = namespace list

(** A type of multiname *)
type multiname = [
  `QName of namespace * string
| `Multiname of string * namespace_set
]

type entry = [
| `Int of int
| `UInt of int
| `Double of float
| `String of string
| namespace
| multiname
]

type t

val empty : t
val add : [< entry] -> t -> t
val index : [< entry] -> t -> int
val to_abc : t -> AbcType.cpool
