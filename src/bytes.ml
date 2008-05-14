open Base
type t = 
    U8 of int 
  | U16 of int 
  | S24 of int 
  | U30 of int32
  | U32 of int32
  | S32 of int32
  | D64 of float

let u8 n = U8 n
let u16 n = U16 n
let u30 n = U30 (Int32.of_int n)
let u32 n = U32 (Int32.of_int n)
let s32 n = S32 (Int32.of_int n)

let (&/) = Int32.logand
let (|/) = Int32.logor
let (>>) = Int32.shift_right_logical

let of_int_list = function
    U8  x when x <= 0xFF -> 
      [x]
  | U16 x when x <= 0xFFFF -> 
      [x land 0xFF; (x lsr 8) land 0xFF ]
  | U30 x | U32 x | S32 x -> 
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
  | _ ->
      invalid_arg "of_int_list"

let rec output_bytes ch = function 
    x::xs -> 
      List.iter (output_byte ch) @@ of_int_list x;
      output_bytes ch xs
  | _ ->
      ()
