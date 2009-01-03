open Base

type 'a t = {
  value:    'a;
  filename: string;
  lineno:   int;
  start_pos: int;
  end_pos:   int;
}

let inc r =
  let old =
    !r in
    incr r;
    old

let map f stream =
  Stream.from
    (fun _ ->
       try
	 Some (f @@ Stream.next stream)
       with Stream.Failure ->
	 None)

let without_line stream =
  map (fun {value=v} -> v) stream

let with_line filename =
  let lineno =
    ref 0 in
  let pos =
    ref 0 in
    map (fun c ->
	   let node = {
	     value    = c;
	     filename = filename;
	     lineno   = !lineno;
	     start_pos= !pos;
	     end_pos  = !pos + 1
	   } in
	     if c = '\n' then begin
	       incr lineno;
	       pos := 0;
	     end else begin
	       incr pos
	     end;
	     node)

let of_string str =
  with_line "<string>" @@
    Stream.of_string str

let of_channel path ch =
  with_line path @@
    Stream.of_channel ch

let of_file path =
  of_channel path @@ open_in path

let value {value=v} =
  v

let empty a =
  {value=a; filename="<empty>"; lineno=0; start_pos=0; end_pos=1}

let ghost a =
  {value=a; filename="<ghost>"; lineno=0; start_pos=0; end_pos=0}

let lift f ({value=x} as node) =
  {node with
     value = f x}

let concat f =
  function
      (x::_) as xs ->
	{x with
	   value = f @@ List.map value xs}
    | [] ->
	empty (f [])

let to_string show {value=value;filename=filename; lineno=lineno; start_pos=a; end_pos=b} =
  Printf.sprintf "%s (%s:%d:%d-%d)"
    (show value) filename lineno a b
