type t
val connect : string -> int -> t
val close : t -> unit
val send : t -> string -> unit
val recv : t -> int -> string
