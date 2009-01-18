(** intermediate code *)

(**{6 Type}*)

(** type for intermediate code *)
type inter_code

(**{6 Conversion}*)

val of_program : Ast.program -> inter_code
val to_program : inter_code  -> Ast.program

(**{6 Binding check}*)
val mem_variable : string*string -> inter_code -> bool
val mem_method : string -> inter_code -> bool

(**{6 File I/O}*)
val output : out_channel -> inter_code -> unit
val input  : in_channel  -> inter_code
