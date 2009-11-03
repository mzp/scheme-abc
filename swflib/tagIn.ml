module type Abc = sig
  type t
  val read : int Stream.t -> t
end

exception UnknownTag of int
module Make(Abc : Abc) = struct
  open Base
  open SwfBaseIn
  open ExtString

  type t = Abc.t TagType.t

  let stream tag s =
    Stream.from begin function
	0 -> Some tag
      | _ ->
	  try
	    Some (Stream.next s)
	  with _ ->
	    None
    end

  let tag n stream =
    match Stream.peek stream with
	Some m when m = n ->
	  Stream.next stream
      | _ ->
	  raise Stream.Failure

  let option f stream =
    try
      Some (f stream)
    with Stream.Failure ->
      None

  let rec many parse stream =
    match stream with parser
	[< e = parse; s>] -> e::many parse s
      | [<>] -> []

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

  let pair f g = parser
      [< x = f; y = g >] ->
	(x,y)

  let alist = parser
      [< count = ui16; xs = repeat count (pair ui16 str) >] ->
	xs

  let bit_bool n s =
    ub n s = 1

  let parse : int Stream.t -> Abc.t TagType.t = parser
      [< _ = tag 0 >]->
	`End
    | [< _ = tag 9; c = rgb >]->
	`SetBackgroundColor c
    | [< _ = tag 43; name = str; anchor = option (tag 1) >] ->
	`FrameLabel (name,anchor <> None)
    | [< _ = tag 24 >] ->
	`Protect
    | [< _ = tag 56; xs = alist >] ->
	`ExportAssets xs
    | [< _ = tag 57; url = str; xs = alist >] ->
	`ImportAssets (url,xs)
    | [< _ = tag 58; passwd = str >] ->
	`EnableDebugger passwd
    | [< _ = tag 64; _ = ui16; passwd = str >] ->
	`EnableDebugger2 passwd
    | [< _ = tag 65; max_rec = ui16; timeout = ui16 >] ->
	`ScriptLimits (max_rec, timeout)
    | [< _ = tag 66; depth = ui16; index = ui16 >] ->
	`SetTabIndex (depth, index)
    | [< _ = tag 69; (is_metadata, is_as3, use_network) = bits ~f:parser
	     [< _ = ub 3; is_metadata = bit_bool 1; is_as3 = bit_bool 1; _ = ub 2; use_network = bit_bool 1; _ = ub 24 >] ->
	       (is_metadata, is_as3, use_network) >] ->
	open TagType in
	  `FileAttributes { is_metadata; is_as3; use_network }
    | [< _ = tag 71; url = str; _ = ui8; _ = ui8; xs = alist >] ->
	`ImportAssets2 (url,xs)
    | [< _ = tag 76; xs = alist >] ->
	`SymbolClass xs
    | [< _ = tag 77; s = str >] ->
	`Metadata s
    | [< _ = tag 78; id = ui16; (left,right,top,bottom) = rect >] ->
	open SwfType in
	  `DefineScalingGrid (id, {left;right;top;bottom})
    | [< _ = tag 86;
	 scene_count = eui32; xs = repeat_l scene_count (pair eui32 str);
	 frame_count = eui32; ys = repeat_l frame_count (pair eui32 str); >] ->
	`DefineSceneAndFrameLabelData (xs, ys)
    | [< _ = tag 1 >] ->
	`ShowFrame
    | [< _ = tag 82; lazyInit = ui32; name = str; data = Abc.read>] ->
	`DoABC (lazyInit = 1l, name, data)
    | [< _ = tag 63; uuid = many Stream.next >] ->
	`DebugID (String.implode @@ List.map Char.chr uuid)
    | [< _ = tag 41;
	 product_id = ui32;
	 edition = ui32;
	 major = ui8;
	 minor = ui8;
	 build_number = ui64;
	 compile_date = ui64 >] ->
	open TagType in
	`ProductInfo { product_id;
		       edition;
		       major;
		       minor;
		       build_number;
		       compile_date }
    | [< tag = Stream.next >] ->
	raise (UnknownTag tag)

  let read tag s =
    parse @@ stream tag s
end
