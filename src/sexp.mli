(** S-expression parsing. *)

type lisp = Int of int | String of string | Float of float | Bool of bool | Symbol of string | List of lisp list


val parse : char Stream.t -> lisp list
val parse_string : string -> lisp list

type t =
    Int_    of int Node.t
  | String_ of string Node.t
  | Float_  of float Node.t
  | Bool_   of bool Node.t
  | Symbol_ of string Node.t
  | List_   of t list Node.t

val of_stream : char Node.t Stream.t -> t list
val of_string : string -> t list
val to_string : t -> string
