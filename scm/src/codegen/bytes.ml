open Base
exception Out_of_range

type address = int
type map = (Label.t * address) list

type base = [
  `U8  of int
| `U16 of int
| `S24 of int
| `U30 of int32
| `U32 of int32
| `S32 of int32
| `D64 of float ]

type label = [
| `Backpatch   of int * (address -> map -> int list) (* (size,fun current_address map -> [...]) *)
| `Label of Label.t ]

type t = [ base | label ]

let u8 n =
  if 0 <=n && n <= 0xFF then
    `U8 n
  else
    raise Out_of_range

let u16 n =
  if 0 <= n && n <= 0xFFFF then
    `U16 n
      else
    raise Out_of_range

let u30 n =
  `U30 (Int32.of_int n)
let u32 n =
  `U30 (Int32.of_int n)
let s32 n =
  `S32 (Int32.of_int n)
let s24 n =
  `S24 n
let d64 f =
  `D64 f

let label x =
  `Label x

let backpatch size f =
  `Backpatch (size,f)

(** encode "base" to bytes *)
let (&/) = Int32.logand
let (|/) = Int32.logor
let (>>) = Int32.shift_right_logical

let split_byte nth value size =
  List.map (fun i-> nth value (i*8)) @@ range 0 size

let split_byte_int =
  split_byte (fun n i-> (n lsr i) land 0xFF)

let split_byte_int64 value size =
  List.map Int64.to_int @@
    split_byte
       (fun n i->(Int64.logand (Int64.shift_right_logical n i) 0xFFL))
       value size

let rec of_base : base -> int list =
  function
      `U8  x ->
	split_byte_int x 1
    | `U16 x ->
	split_byte_int x 2
    | `S24 x ->
	split_byte_int x 3
    | `D64 f ->
	split_byte_int64 (Int64.bits_of_float f) 8
    | `U30 x | `U32 x | `S32 x ->
	if x = 0l then
	  [0]
	else
	  unfold
	    (fun x ->
	       if x = 0l then
		 None
	       else if 0l < x && x <= 0x7Fl then
		 Some (Int32.to_int (x &/ 0x7Fl),0l)
	       else
		 let next =
		   x >> 7 in
		 let current =
		   Int32.to_int ((x &/ 0x7Fl) |/ 0x80l) in
		   Some (current,next)) x

let rec of_label addr map =
  function
      [] ->
	(fun _ -> []),map
    | `Label t::xs ->
	let f,map' =
	  of_label addr ((t,addr)::map) xs in
	  f,map'
    | `Backpatch (size, patch)::xs ->
	let f,map' =
	  of_label (addr+size) map xs in
	  (fun m -> patch addr m @ f m),map'
    | #base as base::xs ->
	let bytes =
	  of_base base in
	let f,map' =
	  of_label (addr + List.length bytes) map xs in
	  (fun m -> bytes @ f m),map'

let find : map -> Label.t -> address  = flip List.assoc

let label_ref label =
  backpatch 3 (fun addr m -> of_base @@ `S24 (find m label - (addr + 3)))

let to_int_list xs =
  let f,map =
    of_label 0 [] xs in
    f map

let rec output_bytes ch bytes =
  bytes
  +> to_int_list
  +> List.iter (output_byte ch)
