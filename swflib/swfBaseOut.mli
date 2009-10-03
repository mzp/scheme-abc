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
  | Float16 of float
  | Float32 of float
  | Float64 of float


val to_int_list : t list -> int list

