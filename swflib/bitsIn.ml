open Base

type byte = int
type bit = int

let rec bits_of_byte_iter n bits byte =
  if n = 0 then
    bits
  else
    bits_of_byte_iter (n-1) ((byte land 1)::bits) (byte lsr 1)

let bits_of_byte n =
  if n > 0xFF then
    raise (Invalid_argument "BitsIn.split");
  bits_of_byte_iter 8 [] n

let of_stream s =
  let cache =
    ref [] in
  Stream.from begin fun _ ->
    try
      if !cache = [] then
	cache := bits_of_byte @@ Stream.next s;
      match !cache with
	  x::xs ->
	    cache := xs;
	    Some x
	| [] ->
	    failwith "must not happen"
    with Stream.Failure _ ->
      None
  end

let bits n s =
  let rec loop value n =
    if n = 0 then
      value
    else
      let value' =
	(Stream.next s) lor (value lsl 1) in
	loop value' @@ n - 1  in
    loop 0 n
