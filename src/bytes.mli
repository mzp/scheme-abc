type t = 
    U8 of int 
  | U16 of int 
  | S24 of int 
  | U30 of int 
  | U32 of int 
  | S32 of int 
  | D64 of float

val of_int_list : t -> int list
val output_bytes: out_channel -> t list -> unit
