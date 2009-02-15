type 'a t

val (=>) : 'string -> 'string -> 'string * 'string
val ($$) : string * string -> ('a -> string -> string -> string list) -> 'a t

val commands : 'a t list -> 'a -> string -> string -> string list
