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
| `PriavteNamespace of string ]

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
| multiname
]

type t

val empty : t
val add : t -> entry -> t
val index : t -> entry -> int
val to_abc : t -> Abc.cpool
