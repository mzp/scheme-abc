open Base

let fail () =
  raise Stream.Failure

let rec times f =
  function
      0 -> ()
    | n -> f () ;times f (n-1)

let rec repeat n f stream =
  if n = 0 then
    []
  else
    match stream with parser
	[<c = f>] ->
	  c::repeat (n-1) f stream
      | [<>] ->
	  raise (Stream.Error "invalid format")

let repeat_l n f stream =
  repeat (Int32.to_int n) f stream



  
let string str stream = 
  let cs =
    ExtString.String.explode str in
  let n = 
    List.length cs in
    match Stream.npeek n stream with
	ys when cs = ys ->
	  times (fun ()->Stream.junk stream) n;
	  ys
      | _ ->
	  fail ()

let char c stream =
  match Stream.peek stream with
      Some x when x = c ->
	Stream.junk stream;
	x
    | _ ->
	fail ()

let rec until c stream =
  match Stream.peek stream with
      Some x when x != c ->
	Stream.junk stream;
	x::(until c stream)
    | _ ->
	[]

let one_of str stream =
  match Stream.peek stream with
      Some c when String.contains str c ->
	Stream.next stream
    | _ ->
	fail ()

let option f stream =
  try
    Some (f stream)
  with Stream.Failure ->
    None

let (<|>) f g = 
  parser 
      [<e = f>] -> e
    | [<e = g>] -> e

let rec many parse stream = 
  match stream with parser
      [< e = parse; s>] -> e::many parse s
    | [<>] -> []

let many1 parse stream =
  let x =
    parse stream in
    x::many parse stream

let alpha stream = 
  match Stream.peek stream with
      Some ('a'..'z') | Some ('A'..'Z') ->
	Stream.next stream
    | _ ->
	fail ()

let digit stream =
  match Stream.peek stream with
      Some ('0'..'9') ->
	Stream.next stream
    | _ ->
	fail ()

let try_ f stream =
  (* 
     Use black-magic to save stream state
     
     from stream.ml:
     type 'a t = { count : int; data : 'a data }
  *)
  let t =
    Obj.repr stream in
  let count =
    Obj.field t 0 in
  let data =
    Obj.field t 1 in
    try
      f stream
    with Stream.Failure | Stream.Error _ ->
      Obj.set_field t 0 count;
      Obj.set_field t 1 data;
      fail ()
