type 'a t
val add : 'a -> 'a t -> 'a t
val add_list : 'a list -> 'a t -> 'a t
val index : 'a -> 'a t -> int
val to_list : 'a t -> 'a list
val empty : 'a t
val mem : 'a -> 'a t -> bool
