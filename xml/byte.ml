open Base

let of_channel ch =
  Stream.from (fun _ ->
		 try
		   Some (input_byte ch)
		 with End_of_file ->
		   None)

let (++) x y =
  (x lsl 8) + y

let byte =
  Stream.next

let u8 =
  parser [<c = byte>] -> c

let u16 =
  parser [<n2 = byte; n1 = byte >] ->
    n1 ++ n2

let size =
  Sys.word_size - 24 - 1

let s_extend d =
  (d lsl size) asr size

let s24 =
  parser [<n3 = byte; n2 = byte; n1 = byte>] ->
    s_extend (n1 ++ n2 ++ n3)

let leq n stream =
  match Stream.peek stream with
      Some m when m <= n ->
	Stream.next stream
    | _ ->
	raise Stream.Failure

let (+++) x y =
  Int32.logor (Int32.shift_left x 7) (Int32.logand y 0x7Fl)

let rec read_u30 stream =
  match stream with parser
      [<n = leq 0x7F >] ->
	Int32.of_int n
    | [<n = byte>] ->
	(read_u30 stream) +++ (Int32.of_int n)
    | [<>] ->
	raise (Stream.Error "invalid format")

let u30 =
  read_u30

let u32 =
  read_u30

let s32 =
  read_u30

let d64 =
  let shift_or x y =
    Int64.logor (Int64.shift_left y 8) (Int64.of_int x) in
    parser
	[<d = Parsec.repeat 8 byte>] ->
	  Int64.float_of_bits @@ List.fold_right shift_or d 0L

let sample () =
  Stream.of_list @@ range 0 10
