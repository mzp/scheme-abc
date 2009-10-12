type byte = int
type bit = int

val of_stream : byte Stream.t -> bit Stream.t
val bits      : int -> bit Stream.t -> int

