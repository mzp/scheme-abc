(* pool for literal *)
type 'a t
type 'a map

val empty   : 'a t
val append  : 'a t -> 'a t -> 'a t
val uniq_by : ('a -> 'a -> int) -> 'a t -> 'a t
val uniq    : 'a t -> 'a t
val add     : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val to_map  : 'a t -> 'a map

val get : 'a -> 'a map -> int
