(** intermediate code *)

type table

val mem_variable : string list*string -> table -> bool
val mem_method : string -> table -> bool

val empty : table

val write : string -> Ast.program -> unit
val load_program : string -> Ast.program
val add_program : table -> string -> Ast.program -> table
val add_dir : table -> string -> table
