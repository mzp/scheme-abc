type bind = Scope of int  | Register of int
type t
val empty_env : t
val add_scope : string list -> t -> t
val add_current_scope : string -> t -> t
val add_register : string list -> t -> t
val get_bind : string -> t -> bind
val get_bind_sure : string -> t -> bind option 
val is_bind : string -> t -> bool
val ensure_scope : string -> t -> int

