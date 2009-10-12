
val ui8  : int Stream.t -> int
val ui16 : int Stream.t -> int
val ui24 : int Stream.t -> int
val ui32 : int Stream.t -> int32

val si8  : int Stream.t -> int
val si16 : int Stream.t -> int
val si24 : int Stream.t -> int
val si32 : int Stream.t -> int32

val eui32 : int Stream.t -> int32

val bits : (BitsIn.bit Stream.t -> 'a) -> int Stream.t -> 'a
val ub : int -> BitsIn.bit Stream.t -> int
val sb : int -> BitsIn.bit Stream.t -> int

val fixed  : int Stream.t -> float
val fixed8 : int Stream.t -> float

val float32  : int Stream.t -> float
val float64 : int Stream.t -> float


