val fail  : unit -> 'a
val times : (unit -> 'a) -> int -> unit
val repeat : int -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list
val repeat_l : int32 -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list
val try_ : ('a -> 'b) -> 'a -> 'b
val ( <|> ) : ('a Stream.t -> 'b) -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'b
val option : ('a -> 'b) -> 'a -> 'b option
val many : ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list
val many1 : ('a Stream.t -> 'b) -> 'a Stream.t -> 'b list
val until : 'a -> 'a Stream.t -> 'a list
val untilBy : ('a -> bool) -> 'a Stream.t -> 'a list
val char : 'a -> 'a Stream.t -> 'a

val string : string -> char Stream.t -> char list
val one_of : string -> char Stream.t -> char
val alpha : char Stream.t -> char
val digit : char Stream.t -> char

module CharS :
  sig
    val string : string -> char Stream.t -> char list
    val one_of : string -> char Stream.t -> char
    val alpha : char Stream.t -> char
    val digit : char Stream.t -> char
  end

module NodeS :
  sig
    val string : string -> char Node.t Stream.t -> char list Node.t
    val one_of : string -> char Node.t Stream.t -> char Node.t
    val alpha : char Node.t Stream.t -> char Node.t
    val digit : char Node.t Stream.t -> char Node.t
  end
