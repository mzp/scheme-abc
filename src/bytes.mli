type t

val of_int_list : t -> int list
val output_bytes: out_channel -> t list -> unit
val backpatch : t list -> int list

val u8  : int -> t
val u16 : int -> t
val u30 : int -> t
val u32 : int -> t
val s32 : int -> t
val s24 : int -> t

val label : Label.t -> t
val label_ref : Label.t -> t
val label_to : (int->t) -> Label.t -> t
