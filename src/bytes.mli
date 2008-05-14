type t

val of_int_list : t -> int list
val output_bytes: out_channel -> t list -> unit

val u8  : int -> t
val u16 : int -> t
val u30 : int -> t
val u32 : int32 -> t
val s32 : int32 -> t

