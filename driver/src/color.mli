type t = {red:int; green:int; blue:int; alpha:float}
exception Invalid_format of string

val red : t
val lime  : t
val blue  : t
val white  : t
val maroon  : t
val green   : t
val navy  : t
val silver  : t
val yellow  : t
val aqua  : t
val fuchsia  : t
val gray  : t
val olive  : t
val teal  : t
val purple  : t
val black  : t

val parse : string -> t
val of_int : int -> t
val to_int : t   -> int

