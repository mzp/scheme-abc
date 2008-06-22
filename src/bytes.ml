open Base

type t = 
    U8 of int 
  | U16 of int 
  | S24 of int 
  | U30 of int32
  | U32 of int32
  | S32 of int32
  | D64 of float
  | Ref of Label.t
  | RefU30 of Label.t
  | Label of Label.t

let u8 n = U8 n
let u16 n = U16 n
let u30 n = U30 (Int32.of_int n)
let u32 n = U32 (Int32.of_int n)
let s32 n = S32 (Int32.of_int n)
let s24 n = S24 n
let d64 f = D64 f
let label x = Label x
let label_ref label = Ref label
let label_u30 xs = RefU30 xs

let (&/) = Int32.logand
let (|/) = Int32.logor
let (>>) = Int32.shift_right_logical

let encode n size =
  List.map (fun i-> (n lsr (i*8)) land 0xFF) @@ range 0 size

let rec of_int_list = function
    U8  x when x <= 0xFF -> 
      encode x 1
  | U16 x when x <= 0xFFFF -> 
      encode x 2
  | S24 x ->
      encode x 3
  | D64 f ->
      let r =
	Obj.repr f in
      let up : int =
	int_of_string @@ string_of_int @@ Obj.obj @@ Obj.field r 0 in
      let down : int = 
	int_of_string @@ string_of_int @@ Obj.obj @@ Obj.field r 1 in
	[0;0;0xf0;0x3f] @ [00;0;0;0]
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

let collect xs =
  let encode (code,table,adr) = 
    function
	Label label -> 
	  (code,(label,adr)::table,adr) 
      | Ref label ->
	  (`Ref (label,adr+3)::code,table,adr+3)
      | RefU30 label ->
	  (`RefU30 (label,adr)::code,table,adr)
      | byte ->
	  let ints =
	    of_int_list byte in
	  let n =
	    List.length ints in
	  (`Ints ints::code,table,adr+n) in
  let code,table,_ = 
    List.fold_left encode ([],[],0) xs in
    table,List.rev code
    
let backpatch bytes =
  let table,ints =
    collect bytes in
  let patch = 
    function
	`Ints x -> x
      | `Ref (label,adr) -> 
	  of_int_list (S24 ((List.assoc label table)-adr)) 
      | `RefU30 (label,adr) -> 
	  of_int_list (U30 (Int32.of_int @@ List.assoc label table - adr)) in
    concat_map patch ints
  
let rec output_bytes ch bytes = 
  let ints =
    backpatch bytes in
    List.iter (output_byte ch) ints

let get (x : float) y : float = 
  let r =
    Obj.repr x in
  let f =
    Obj.field r y in
    Obj.obj f
