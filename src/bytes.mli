type t

val u8  : int -> t
val u16 : int -> t
val u30 : int -> t
val u32 : int -> t
val d64 : float -> t
val s32 : int -> t
val s24 : int -> t

val label : Label.t -> t
val label_ref : Label.t -> t

val block : t list -> t

val output_bytes: out_channel -> t list -> unit
val to_int_list : t list -> int list

