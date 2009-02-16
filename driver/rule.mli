type filetype = string
type filename = string
type ('a,'b) cmd = 'a -> 'b -> filename -> string list
type 'a t

val one_to_one : filetype -> filetype -> ('a,filename) cmd -> 'a t
val many_to_one : filetype list -> filetype -> ('a,filename list) cmd -> 'a t
val comands : 'a -> 'a t list -> filename list -> filename -> string list
