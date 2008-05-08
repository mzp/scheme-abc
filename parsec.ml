open Base

let fail () =
  raise Stream.Failure

let rec times f =
  function
      0 -> ()
    | n -> f () ;times f (n-1)
  
let string str stream = 
  let cs =
    explode str in
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
