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
  | Fixed of float
  | Fixed8 of float
  | Float16 of float
  | Float32 of float
  | Float64 of float

let rec g_si mask shift n value =
  unfold begin fun (n,value) ->
    if n = 0 then
      None
    else
      Some (mask value,(n-1,shift value))
  end (n,value)

let si n value =
  g_si ((land) 0xff) (fun x -> x lsr 8) n value

let rec to_int_list xs =
  HList.concat_map encode xs
and encode = function
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
  | Fixed x ->
      let int =
	floor x in
      let decimal =
	(x -. int) *. float 0x1_00_00 in
	to_int_list [Ui16 (int_of_float decimal);
		     Ui16 (int_of_float int)]
  | Fixed8 x ->
      let int =
	floor x in
      let decimal =
	(x -. int) *. float 0x1_00 in
	to_int_list [Ui8 (int_of_float decimal);
		     Ui8 (int_of_float int)]
  | Float16 x ->
      (*
	[single precision]
	Sign bit: 1 bit
	Exponent width: 8 bits
	Significand precision: 23 bits

	[half presicion]
	Sign bit: 1 bit
	Exponent width: 5 bit
	Significand precision: 10 bits
      *)
      let single =
	Int32.bits_of_float x in
      let sign =
	Int32.to_int @@ Int32.shift_right_logical single 31 in
      let exp =
	Int32.to_int @@ Int32.logand 0b1_1111l @@ Int32.shift_right_logical single 23 in
      let prec =
	Int32.to_int @@ Int32.logand 0b11_1111_1111l @@ single in
      let half =
	(sign lsl 15) lor ((exp + 127 - 15) lsl 10) lor prec in
	encode @@ Ui16 half
  | Float32 x ->
      encode @@ Ui32 (Int32.bits_of_float x)
  | Float64 x ->
      encode @@ Ui64 (Int64.bits_of_float x)
