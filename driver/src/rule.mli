type filetype = string
type filename = string
type ('a,'b) cmd = 'a -> 'b -> filename -> string list
type 'a t

val one_to_one : filetype -> filetype -> ('a,filename) cmd -> 'a t
val many_to_one : filetype list -> filetype -> ('a,filename list) cmd -> 'a t
val commands : 'a -> 'a t list -> filename list -> filename -> string list
val temp_files : 'a -> 'a t list -> filename list -> filename -> string list
