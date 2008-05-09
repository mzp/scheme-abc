type lisp = String of string | Symbol of string | List of lisp list
val read : char Stream.t -> lisp list
val read_string : string -> lisp list
