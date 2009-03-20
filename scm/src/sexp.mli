(** S-expression parsing. *)
type t =
    Int    of int Node.t
  | String of string Node.t
  | Float  of float Node.t
  | Bool   of bool Node.t
  | Symbol of string Node.t
  | List   of t list Node.t

val of_stream : char Node.t ExtStream.Stream.t -> t list
val of_string : string -> t list
val to_string : t -> string
