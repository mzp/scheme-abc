type 'a t
val singleton : 'a -> 'a t
val add   : 'a   -> 'a t -> 'a t
val diff  : 'a t -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val mem     : 'a -> 'a t -> bool
val empty   : 'a t
val set_of_list : 'a list -> 'a t
