type bit =
    SB of int * int
  | UB of int * int

type s = [
  `Si8     of int
| `Si16    of int
| `Si24    of int
| `Si32    of int32
| `Ui8     of int
| `Ui16    of int
| `Ui24    of int
| `Ui32    of int32
| `Ui64    of int64
| `EUi32   of int32
| `Bits    of bit list
| `Fixed   of float
| `Fixed8  of float
| `Float32 of float
| `Float64 of float
| `Rect    of int*int*int*int
| `RGB     of int * int * int
| `RGBA    of int * int * int * int
]

type backpatch = [
| `Ui32Size
| `Size    of (int -> s list) * s list
]

type t = [ s | backpatch ]

val to_list : t list -> int list
