val of_channel : in_channel -> int Stream.t
val u8 : 'a Stream.t -> 'a
val u16 : int Stream.t -> int
val s24 : int Stream.t -> int
val u30 : int Stream.t -> int32
val u32 : int Stream.t -> int32
val s32 : int Stream.t -> int32
val d64 : int Stream.t -> float

