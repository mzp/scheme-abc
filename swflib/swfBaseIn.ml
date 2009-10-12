open Base

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

let bits f s =
  f @@ BitsIn.of_stream s

let ub n bs =
  BitsIn.bits n bs

let sb n bs =
  s_extend n (ub n bs)

let fb _ _ =  undefined
