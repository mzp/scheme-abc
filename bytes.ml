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
    U8  x -> 
      [x]
  | U16 x -> 
      [x land 0xFF; (x lsr 8) land 0xFF ]
  | U30 x | U32 x -> 
      unfold 
	(fun x -> 
	   if x = -1 then None 
	   else if x < 0x80 then
	     Some (x land 0x7F,-1)
	   else
	     Some ((x land 0x7F) lor 0x80,x lsr 7))
	x

let rec output_bytes ch = function 
    x::xs -> 
      List.iter (output_byte ch) @@ of_int_list x;
      output_bytes ch xs
  | _ -> 
      ()

let test () = 
  let ch = 
    open_out_bin "test.abc" in
    output_bytes ch [
      U16 16; (* minor version *)
      U16 46; (* major_version *)
      
      (* constant pool *)
      U30 0; (* int  count *)
      U30 0; (* uint count *)
      U30 0; (* double count *)
      U30 0; (* string_count *)
      U30 0; (* namespace_count *)
      U30 0; (* ns set count *)
      U30 0; (* multiname count *)

      (* method info *)
      U30 1; (* method count *)
      U30 0; (* param count *)
      U30 0; (* return type *)
      U30 0; (* name *)
      U8 0;  (* flags *)
      U30 0; (* metadata count *)
      U30 0; (* class count *)

      (* script *)
      U30 1; (* script count *)
      U30 0; (* init *)
      U30 0; (* trait count *)

      (* method body *)
      U30 1; (* method body count *)
      U30 0; (* method *)
      U30 1; (* max stack *)
      U30 1; (* local count *)
      U30 0; (* init scope depth *)
      U30 1; (* max scope depth *)

      (* code *)
      U30 3; (* code length *)
      U8 0xD0; (* getlocal_0 *)
      U8 0x30; (* pushscope *)
      U8 0x47; (* return_void *)
      
      U30 0; (* exception count *)
      U30 0; (* trait count *)
    ];
    close_out ch
