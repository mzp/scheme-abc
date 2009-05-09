(** 
    ABC primitive data type.

    Provide the type of ABC primitive data type and byte encodeing function.

    @author mzp
    @see <http://www.adobe.com/devnet/actionscript/articles/avm2overview.pdf> AVM2 Overview(pdf)
*)

(** the type of a primitive data type *)
type t

exception Out_of_range

(** {6 Create data type }*)

(** create u8

@raise InvalidArgumnet If n > 0xFF or n < 0. *)
val u8  : int -> t

(** create u16

@raise InvalidArgumnet If n > 0xFFFF or n < 0. *)
val u16 : int -> t

(** create u30 *)
val u30 : int -> t

(** create u32 *)
val u32 : int -> t

(** create d64 *)
val d64 : float -> t

(** create s32 *)
val s32 : int -> t

(** create s24 *)
val s24 : int -> t

(**{6 Label}*)

(** 
    [label l] create label. This label is refered by [lable_ref l].

    This value is removed when encode.
*)
val label : Label.t -> t

(**
   [label_ref l] refer to [label l] position.

   This value become s24 when encode.
*)
val label_ref : Label.t -> t

(**{6 Block}*)

(**
   [block xs] create block.

   Block become [(u30 size_of_xs)::xs ] when encode.
*)
val block : t list -> t

(**{6 Encode}*)

(**
   [to_int_list xs] encode [xs] to [int list].
*)
val to_int_list : t list -> int list

(**
   [output_bytes ch xs] output encoded [xs] to [ch]
*)
val output_bytes: out_channel -> t list -> unit
