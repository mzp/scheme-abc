exception Out_of_range

type 'a m
val bind : 'a m -> ('a -> 'b m) -> 'b m
val ret : 'a -> 'a m
val runWriter : 'a m -> 'a * string

val db : int -> unit m
val dw : int -> unit m
val dd : Int32.t -> unit m
val sz : string -> unit m
