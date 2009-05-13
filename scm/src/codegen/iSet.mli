type 'a t
val add : 'a t -> 'a -> 'a t
val add_list : 'a t -> 'a list -> 'a t
val index : 'a t -> 'a -> int
val to_list : 'a t -> 'a list
val empty : 'a t
