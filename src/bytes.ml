open Base
type t = 
    U8 of int 
  | U16 of int 
  | S24 of int 
  | U30 of int 
  | U32 of int 
  | S32 of int 
  | D64 of float

let of_int_list = function
    U8  x when x <= 0xFF -> 
      [x]
  | U16 x when x <= 0xFFFF -> 
      [x land 0xFF; (x lsr 8) land 0xFF ]
  | U30 x | U32 x -> 
      if x = 0 then
	[0]
      else
	unfold 
	  (fun x -> 
	   if x = 0 then 
	     None
	   else if 0 < x && x <= 0x7F then
	     Some ((x land 0x7F),0)
	   else
	     Some ((x land 0x7F) lor 0x80,x lsr 7)) x
  | _ ->
      invalid_arg "of_int_list"

let rec output_bytes ch = function 
    x::xs -> 
      List.iter (output_byte ch) @@ of_int_list x;
      output_bytes ch xs
  | _ ->
      ()
