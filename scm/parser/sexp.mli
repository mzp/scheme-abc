(** S-expression parsing. *)
type t =
    Int    of int Node.t
  | String of string Node.t
  | Float  of float Node.t
  | Bool   of bool Node.t
  | Symbol of string Node.t
  | List   of t list Node.t

val of_stream : Token.t Stream.t -> t list

