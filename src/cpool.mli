(** 
    Constant pool(CPool).

    CPool create the map from a value to the index, or a list of the value.
 *)

(** A type of constant pool *)
type t

(** A type of namespace *)
type namespace = 
    Namespace of string 
  | PackageNamespace of string
type namespace_set = namespace list

(** A type of multiname *)
type multiname = 
    QName of namespace * string 
  | Multiname of string * namespace_set

val to_string : t -> string

(**{6 Create CPool}*)

(** empty cpool *)
val empty : t

val make_qname : ?ns:string -> string -> multiname

(** [append x y] creates new cpool which include [x] and [y].*)
val append : t -> t -> t

(** [int n] creates new cpool which include only [n]. *)
val int:       int       -> t

(** [unt n] creates new cpool which include only [n]. *)
val uint:      int       -> t

(** [double n] creates new cpool which include only [n]. *)
val double:    float     -> t

(** [string n] creates new cpool which include only [n]. *)
val string:    string    -> t

(** [namespace n] creates new cpool which include [n] and its dependency.*)
val namespace: namespace -> t

(** [multiname n] creates new cpool which include [n] and its dependency. *)
val multiname: multiname -> t

(**{6 Encode to ABC}*)
(** [to_abc c] encode cpool to ABC. *)
val to_abc : t -> Abc.cpool

(**{6 Get index}*)
(** These function map value to index. *)

val int_nget : int -> t -> int
val uint_nget : int -> t -> int
val string_nget : string -> t -> int
val double_nget : float -> t -> int
val namespace_nget : namespace -> t -> int
val multiname_nget : multiname -> t -> int

(** These function is warpper to return [Bytes.u30].*)

val int_get : int -> t -> Bytes.t
val uint_get : int -> t -> Bytes.t
val string_get : string -> t -> Bytes.t
val double_get : float -> t -> Bytes.t
val namespace_get : namespace -> t -> Bytes.t
val multiname_get : multiname -> t -> Bytes.t


