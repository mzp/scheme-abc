type 'a t = {
  value:    'a;
  filename: string;
  lineno:   int;
  start_pos: int;
  end_pos:   int;
}

val of_string  : string -> char t Stream.t
val of_file    : string -> char t Stream.t
val of_channel : string -> in_channel -> char t Stream.t

val empty : 'a -> 'a t
val ghost : 'a -> 'a t
val value : 'a t -> 'a

val lift : ('a -> 'b) -> 'a t -> 'b t
val concat : ('a list -> 'b) -> 'a t list -> 'b t
val to_string : ('a -> string) -> 'a t -> string

val report : string -> string t -> unit
