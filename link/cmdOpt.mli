class type t = object
  method color:int*int*int
  method size:int*int
  method main_class:string
  method output:string
  method use_network:bool
end

val parse : string array -> string list * t
val parse_argv : unit -> string list * t
