(** intermediate code *)

(**{6 Type}*)

(** type for intermediate code *)
type inter_code
type table

(**{6 Conversion}*)
val of_program : ModuleTrans.program -> inter_code
val to_program : inter_code  -> ModuleTrans.program

(**{6 Binding check}*)
val mem_variable : string*string -> table -> bool
val mem_method : string -> table -> bool

(**{6 Loading inter code}*)
val empty : table
val add : table -> string -> inter_code -> table
val add_file : table -> string -> table

(**{6 File I/O}*)
val output : out_channel -> inter_code -> unit
val input  : in_channel  -> inter_code

