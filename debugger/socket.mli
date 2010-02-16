type t
val connect : string -> int -> t
val send : t -> string -> unit
val recv : t -> int -> string
