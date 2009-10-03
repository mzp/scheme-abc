open Base

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

let rec g_si mask shift n value =
  unfold begin fun (n,value) ->
    if n = 0 then
      None
    else
      Some (mask value,(n-1,shift value))
  end (n,value)

let si n value =
  g_si ((land) 0xff) (fun x -> x lsr 8) n value

let rec encode = function
    Si8 n  | Ui8  n ->
      si 1 n
  | Si16 n | Ui16 n ->
      si 2 n
  | Si24 n | Ui24 n ->
      si 3 n
  | Si32 n | Ui32 n ->
      g_si
	(fun n -> Int32.to_int @@ Int32.logand 0xFFl n)
	(fun n -> Int32.shift_right n 8)
	4 n
  | Ui64 n ->
      g_si
	(fun n -> Int64.to_int @@ Int64.logand 0xffL n)
	(fun n -> Int64.shift_right n 8)
	8 n


let to_int_list xs =
  HList.concat_map encode xs
