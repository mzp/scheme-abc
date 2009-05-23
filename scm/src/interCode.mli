(** intermediate code *)

type t = private <mem_method: string -> bool; mem_symbol: string list * string -> bool; ..>

val empty : t
val output : string -> t -> unit
val add : string -> Ast.program -> t -> t
val input : string -> t -> t
val input_dir : string -> t -> t

