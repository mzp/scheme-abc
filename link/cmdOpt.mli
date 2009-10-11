type t = {
  size       : int * int;
  color      : int * int * int;
  main_class : string
}
val parse : string array -> string list * t
val parse_argv : unit -> string list * t
