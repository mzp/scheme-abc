
type lisp = Int of int | String of string | Float of float | Bool of bool | Symbol of string | List of lisp list

val parse : char Stream.t -> lisp list
val parse_string : string -> lisp list
