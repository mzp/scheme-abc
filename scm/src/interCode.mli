(** intermediate code *)

type table

val mem_variable : string*string -> table -> bool
val mem_method : string -> table -> bool

val empty : table

val write : string -> ModuleTrans.program -> unit
val add_program : table -> string -> ModuleTrans.program -> table
val add_dir : table -> string -> table

