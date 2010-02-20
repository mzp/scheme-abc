exception Out_of_range
module M : sig
  type w = string
  type 'a m
  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val ret : 'a -> 'a m
  val runWriter : 'a m -> 'a * w
end

val db : int -> unit M.m
val dw : int -> unit M.m
val dd : Int32.t -> unit M.m
val sz : string -> unit M.m
