type 'a t = int Stream.t -> 'a

val ui8  : int t
val ui16 : int t
val ui24 : int t
val ui32 : int32 t

val si8  : int t
val si16 : int t
val si24 : int t
val si32 : int32 t

val eui32 : int32 t

val bits : f:(BitsIn.bit Stream.t -> 'a) -> 'a t
val ub : int -> BitsIn.bit Stream.t -> int
val sb : int -> BitsIn.bit Stream.t -> int

val fixed  : float t
val fixed8 : float t

val float32  : float t
val float64  : float t

val rect : (int * int * int * int) t
val str  : string t
val rgb   : (int * int * int) t
val rgba  : (int * int * int * int) t
val argb  : (int * int * int * int) t
