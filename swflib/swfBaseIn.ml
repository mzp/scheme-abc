open Base
open ExtString

type 'a t = int Stream.t -> 'a

let rec until c stream =
  match Stream.peek stream with
      Some x when x != c ->
	Stream.junk stream;
	x::(until c stream)
    | _ ->
	[]

let byte =
  Stream.next

let (++) x y =
  (x lsl 8) + y

let (+++) x y =
  Int32.add (Int32.shift_left x 8) y

let s_extend size d =
  let n =
    Sys.word_size - size - 1 in
    (d lsl n) asr n

let ui8 =
  parser [< n = byte >] ->
    n

let ui16 =
  parser [< x = byte; y = byte >] ->
    y ++ x

let ui24 =
  parser [< x = byte; y = byte; z = byte >] ->
    z ++ y ++ x

let ui32 =
  parser [< x = byte; y = byte; z = byte; w = byte >] ->
    List.fold_left (+++) 0l @@ List.map Int32.of_int [w;z;y;x]

let ui64 =
  open Int64 in
  parser [< a = byte; b = byte; c = byte; d = byte;
	    e = byte; f = byte; g = byte; h = byte >] ->
    List.fold_left (fun x y -> add (shift_left x 8) y) 0L @@ List.map of_int [h;g;f;e;d;c;b;a]


let si8  s =  s_extend 8  @@ ui8 s
let si16 s =  s_extend 16 @@ ui16 s
let si24 s =  s_extend 24 @@ ui24 s
let si32   =  ui32

let leq n stream =
  match Stream.peek stream with
      Some m when m <= n ->
	Stream.next stream
    | _ ->
	raise Stream.Failure

let rec read_u30 stream =
  let (+++) x y =
    Int32.logor (Int32.shift_left x 7) (Int32.logand y 0x7Fl) in
  match stream with parser
      [<n = leq 0x7F >] ->
	Int32.of_int n
    | [<n = byte>] ->
	read_u30 stream +++ Int32.of_int n
    | [<>] ->
	raise (Stream.Error "invalid format")

let eui32 = read_u30

let bits ~f s =
  f @@ BitsIn.of_stream s

let ub n bs =
  BitsIn.bits n bs

let sb n bs =
  s_extend n (ub n bs)

let fixed =
  parser [< decimal = float $ ui16; int = float $ ui16>] ->
    int +. (decimal /. float 0x1_00_00)

let fixed8 =
  parser [< decimal = float $ ui8; int = float $ ui8 >] ->
    int +. (decimal /. float 0x1_00)

let float32 =
  parser [< d = ui32 >] ->
    Int32.float_of_bits d

let float64 =
  open Int64 in
  parser [< x = of_int32 $ ui32; y = of_int32 $ ui32 >] ->
    float_of_bits @@ logor (shift_left y 32) x

let rect s = bits s ~f:begin parser
    [< n = ub 5; x_min = sb n; x_max = sb n; y_min = sb n; y_max = sb n>] ->
      (x_min, x_max, y_min, y_max)
end

let str s =
  let xs =
    until 0 s in
  let _ =
    (* eat null terminator *)
    Stream.junk s in
    xs
    +> List.map Char.chr
    +> String.implode

let rgb  = parser
    [< r = ui8; g = ui8; b = ui8 >] ->
      (r,g,b)

let rgba = parser
    [< (r,g,b) = rgb; a = ui8 >] ->
      (r,g,b,a)

let argb = rgba
