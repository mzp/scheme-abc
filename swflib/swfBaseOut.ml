open Base
open StdLabels
open SwfType
open ExtString

type bit =
    SB of int * int
  | UB of int * int
  | FB of int * float

type byte = [
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
]

type compose = [
  `Fixed   of float
| `Fixed8  of float
| `Float32 of float
| `Float64 of float
| `Rect    of int * int * int * int
| `RGB     of int * int * int
| `RGBA    of int * int * int * int
| `ARGB    of int * int * int * int
| `Matrix  of matrix
| `Str     of string
| `Lang    of int
]

type s = [ byte | compose ]

type backpatch = [
  `Ui32Size
| `Size    of (int -> s list) * s list
]

type t = [ s | backpatch ]

let rec g_si mask shift n value =
  unfold begin fun (n,value) ->
    if n = 0 then
      None
    else
      Some (mask value,(n-1,shift value))
  end (n,value)

let si n value =
  g_si ((land) 0xff) (fun x -> x lsr 8) n value

let mask n =
  (1 lsl n) - 1

let bits s = function
    UB(width, bits) ->
      BitsOut.put s ~width ~bits
  | SB(width, bits) ->
      if bits < - mask (width - 1) - 1 ||  mask (width - 1) < bits then
	raise (Invalid_argument "SB");
      BitsOut.put s ~width ~bits:(bits land mask width)
  | FB(width, bits) ->
      let int =
	floor bits in
      let decimal =
	(bits -. int) *. float 0x1_00_00 in
      let t =
	BitsOut.put s ~width:16 ~bits:(int_of_float decimal) in
	BitsOut.put t ~width:(width-16) ~bits:(int_of_float int)

let to_int : byte -> int list = function
    `Si8 n  | `Ui8  n ->
      si 1 n
  | `Si16 n | `Ui16 n ->
      si 2 n
  | `Si24 n | `Ui24 n ->
      si 3 n
  | `Si32 n | `Ui32 n ->
      g_si
	(fun n -> Int32.to_int @@ Int32.logand 0xFFl n)
	(fun n -> Int32.shift_right n 8)
	4 n
  | `Ui64 n ->
      g_si
	(fun n -> Int64.to_int @@ Int64.logand 0xffL n)
	(fun n -> Int64.shift_right n 8)
	8 n
  | `EUi32 x ->
      if x = 0l then
	[0]
      else
	unfold begin fun x ->
	  if x = 0l then
	    None
	  else if 0l < x && x <= 0x7Fl then
	    Some (Int32.to_int (Int32.logand x 0x7Fl),0l)
	  else
	    let next =
	      Int32.shift_right x 7 in
	    let current =
	      Int32.to_int @@ Int32.logor 0x80l @@ Int32.logand x 0x7Fl in
	      Some (current,next)
	end x
  | `Bits xs ->
      List.fold_left ~f:bits ~init:BitsOut.empty xs
      +> BitsOut.to_list

let char c =
  `Ui8 (Char.code c)

let bit_width xs =
  let bits =
    float @@ 1 + HList.maximum xs in
    int_of_float @@ 1. +. ceil (log bits /. log 2.)

let to_byte : compose -> byte list = function
    `Fixed x ->
      let int =
	floor x in
      let decimal =
	(x -. int) *. float 0x1_00_00 in
	 [`Ui16 (int_of_float decimal);
	  `Ui16 (int_of_float int)]
  | `Fixed8 x ->
      let int =
	floor x in
      let decimal =
	(x -. int) *. float 0x1_00 in
	[`Ui8 (int_of_float decimal);
	 `Ui8 (int_of_float int)]
  | `Float32 x ->
      [`Ui32 (Int32.bits_of_float x)]
  | `Float64 x ->
      [`Ui64 (Int64.bits_of_float x)]
  | `Rect (x_min,x_max,y_min,y_max) ->
      let w =
	bit_width [x_min; x_max; y_min; y_max] in
	[`Bits [UB(5, w);
		SB(w, x_min); SB(w, x_max);
		SB(w, y_min); SB(w, y_max)]]
  | `RGB(r,g,b) ->
      [`Ui8 r; `Ui8 g; `Ui8 b]
  | `RGBA(r,g,b,a) ->
      [`Ui8 r; `Ui8 g; `Ui8 b; `Ui8 a]
  | `ARGB(a, r, g, b) ->
      [`Ui8 a; `Ui8 r; `Ui8 g; `Ui8 b]
  | `Str s ->
      List.map ~f:char (String.explode s) @ [`Ui8 0]
  | `Lang n ->
      [`Ui8 n]
  | `Matrix {scale; rotate; translate=(x,y) } ->
      let t_w =
	bit_width [x; y] in
      let bits =
	List.concat [ begin match scale with
			  None ->
			    [UB(1,0)]
			| Some (x,y) ->
			    let w =
			      16 + bit_width [int_of_float @@ ceil x; int_of_float @@ ceil y] in
			      [UB(1,1); UB(5,w); FB(w,x); FB(w,y)]
		      end;
		      begin match rotate with
			  None ->
			    [UB(1,0)]
			| Some (skew0, skew1) ->
			    let w =
			      16 + bit_width [int_of_float @@ ceil skew0; int_of_float @@ ceil skew1] in
			      [UB(1,1); UB(5,w); FB(w,skew0); FB(w,skew1)]
		      end;
		      (* translate *)
		      [ UB(5,t_w); SB(t_w,x); SB(t_w,y) ] ] in
	[`Bits bits]

let int_of_compose x =
  match x with
      #compose as c -> HList.concat_map to_int @@ to_byte c
    | #byte    as b -> to_int b

let backpatch (xs : [byte | backpatch] list) : int list =
  let (f,size) =
    List.fold_right xs ~init:(const [],0) ~f:begin fun x (f,size) ->
      match x with
	  #byte as b ->
	    let ints =
	      to_int b in
	    let size' =
	      size + List.length ints in
	      ((fun ctx -> ints @ f ctx), size')
	| `Ui32Size ->
	    let size' =
	      (* same as Ui30 *)
	      size + 4 in
	      ((fun size -> to_int (`Ui32 (Int32.of_int size)) @ f size), size')
	| `Size(g, xs) ->
	    let ints =
	      HList.concat_map int_of_compose xs in
	    let size' =
	      List.length ints in
	    let i =
	      HList.concat_map int_of_compose @@ g size' in
	      ((fun ctx -> i @ ints @ f ctx), List.length i + size'+size)
    end in
    f size

let rec to_list xs =
  xs
  +> HList.concat_map begin function
      #byte      as b  -> [b]
    | #compose   as c  -> (to_byte c :> [ byte | backpatch] list )
    | #backpatch as bp -> [bp]
  end
  +> backpatch

