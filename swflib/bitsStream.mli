type t

val empty : t
val put: t -> width:int -> bits:int -> t
val to_list: t -> int list
