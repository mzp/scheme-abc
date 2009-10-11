type t = <color:int*int*int; size:int*int; main_class:string; output:string>

val parse : string array -> string list * t
val parse_argv : unit -> string list * t
