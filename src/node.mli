type 'a t = {
  value:    'a;
  filename: string;
  lineno:   int;
}

val of_string  : string -> char t Stream.t
val of_file    : string -> char t Stream.t
val of_channel : string -> in_channel -> char t Stream.t

val empty : 'a -> 'a t
val value : 'a t -> 'a


