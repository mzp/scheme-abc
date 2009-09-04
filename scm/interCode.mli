(** intermediate code *)

type t = private <mem_method: string -> bool; mem_symbol: string list * string -> bool; to_ast: Ast.program; mem_module: string list -> bool; ..>

val empty : t
val add :    string -> Ast.program -> t -> t
val input :  string -> string -> t -> t
val output : string -> string -> t -> unit


