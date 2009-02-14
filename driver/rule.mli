type t

val (=>) : 'string -> 'string -> 'string * 'string
val (<>) : string * string -> (string -> string -> string list) -> t

val commands : t list -> string -> string -> string list
