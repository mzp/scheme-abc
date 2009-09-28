type t = int
val u8  : t Stream.t -> int
val u16 : t Stream.t -> int
val s24 : t Stream.t -> int
val u30 : t Stream.t -> int
val u32 : t Stream.t -> int32
val s32: t Stream.t -> int32
val d64: t Stream.t ->float

val of_channel : in_channel -> t Stream.t
val of_list : int list -> t Stream.t
