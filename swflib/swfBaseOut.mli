type bit =
    SB of int * int
  | UB of int * int

type t =
    Si8  of int
  | Si16 of int
  | Si24 of int
  | Si32 of int32
  | Ui8  of int
  | Ui16 of int
  | Ui24 of int
  | Ui32 of int32
  | Ui64 of int64
  | Fixed of float
  | Fixed8 of float
  | Float32 of float
  | Float64 of float
  | EUi32 of int32
  | Bits    of bit list
  | Rect of int*int*int*int


val to_int_list : t list -> int list

