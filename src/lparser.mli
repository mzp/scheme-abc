type lisp = String of string | Symbol of string | List of lisp list
val parse : char Stream.t -> lisp list
val parse_string : string -> lisp list
