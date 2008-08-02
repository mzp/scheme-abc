open Base

type base = 
    U8  of int 
  | U16 of int 
  | S24 of int 
  | U30 of int32
  | U32 of int32
  | S32 of int32
  | D64 of float

type labeled =
    Ref   of Label.t
  | Label of Label.t
  | Base  of base

type blocked =
    Block   of blocked list
  | Labeled of labeled

type t = blocked

let lift_base x =
  Labeled (Base x)

let u8 n    = 
  if 0 <=n && n <= 0xFF then lift_base @@ U8 n
  else invalid_arg "Bytes.u8"

let u16 n   = 
  if 0 <= n && n <= 0xFFFF then lift_base @@ U16 n
  else invalid_arg "Bytes.u16"

let u30 n   = lift_base @@ U30 (Int32.of_int n)
let u32 n   = lift_base @@ U32 (Int32.of_int n)
let s32 n   = lift_base @@ S32 (Int32.of_int n)
let s24 n   = lift_base @@ S24 n
let d64 f   = lift_base @@ D64 f

let label x         = Labeled (Label x)
let label_ref label = Labeled (Ref label)

let block xs = Block xs

(** encode "base" to bytes *)
let (&/) = Int32.logand
let (|/) = Int32.logor
let (>>) = Int32.shift_right_logical

let split_byte extract value size =
  List.map (fun i-> extract value (i*8)) @@ range 0 size

let split_byte_int =
  split_byte (fun n i->(n lsr i) land 0xFF)

let split_byte_int64 value size =
  List.map Int64.to_int @@ split_byte (fun n i->(Int64.logand (Int64.shift_right_logical n i) 0xFFL)) value size

let rec encode_base = function
    U8  x ->
      split_byte_int x 1
  | U16 x ->
      split_byte_int x 2
  | S24 x ->
      split_byte_int x 3
  | D64 f ->
      split_byte_int64 (Int64.bits_of_float f) 8
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

(** encode label *)

(* pass1: collecting label *)
let collect xs =
  let encode (code,table,adr) = 
    function
	Label label -> 
	  (code,(label,adr)::table,adr) 
      | Ref label ->
	  (`Ref (label,adr+3)::code,table,adr+3)
      | Base byte ->
	  let ints =
	    encode_base byte in
	  let n =
	    List.length ints in
	  (`Ints ints::code,table,adr+n) in
  let code,table,_ = 
    List.fold_left encode ([],[],0) xs in
    table,List.rev code
    
(* pass2: back-patching label *)
let backpatch table bytes =
  let patch = 
    function
	`Ints x -> 
	  x
      | `Ref (label,adr) -> 
	  encode_base (S24 ((List.assoc label table)-adr)) 
  in
    concat_map patch bytes

let encode_labeled bytes = 
  let table,xs =
    collect bytes 
  in
    backpatch table xs

(** encode blocked *)
let rec encode_blocked bytes =
  let encode =
    function 
	[Block xs] ->
	  let ys = 
	    encode_blocked xs in
	  let len =
	    encode_base @@ U30 (Int32.of_int @@ List.length ys) in
	    len @ ys
      | xs ->
	  encode_labeled @@ List.map (fun (Labeled x)->x) xs in
  let same x y =
    match x,y with
      | Labeled _,Labeled _ ->
	  true
      | _ ->
	  false in
    concat_map encode @@ group_by same bytes

(** util function *)
let to_int_list xs =
  try
    encode_blocked xs
  with Invalid_argument msg ->
    invalid_arg "Bytes.to_int_list"
	
let rec output_bytes ch bytes = 
  let ints =
    to_int_list bytes in
    List.iter (output_byte ch) ints
