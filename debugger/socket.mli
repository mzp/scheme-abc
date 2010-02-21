type t
val connect : string -> int -> t
val listen : int -> t
val connect_with : f:(t -> 'a ) -> string -> int -> 'a
val listen_with : f:(t -> 'a ) -> int -> 'a
val close : t -> unit
val send : t -> string -> unit
val recv : t -> int -> string
