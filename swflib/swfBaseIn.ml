open Base

let byte =
  Stream.next

let (++) x y =
  (x lsl 8) + y

let (+++) x y =
  Int32.add (Int32.shift_left x 8) (Int32.of_int y)

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
    (Int32.of_int w) +++ z +++ y +++ x

let si8 s =
  s_extend 8 @@ ui8 s

let si16 s =
  s_extend 16 @@ ui16 s
let si24 s =
  s_extend 24 @@ ui24 s

let si32 =
  ui32
