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
	ghost (f [])

let to_string show {value=value;filename=filename; lineno=lineno; start_pos=a; end_pos=b} =
  Printf.sprintf "%s (%s:%d:%d-%d)"
    (show value) filename lineno a b


let rec nth_line n ch =
  if n = 0 then
    input_line ch
  else begin
    ignore @@ input_line ch;
    nth_line (n-1) ch
  end

let report kind { value     = msg;
		  filename  = filename;
		  lineno    = lineno;
		  start_pos = a;
		  end_pos   = b } =
  let ch =
    open_in filename in
    Printf.eprintf "%s:%d: %s, %s\n" filename lineno kind msg;
    prerr_endline @@ nth_line lineno ch;
    for i = 0 to b - 1 do
      if i >= a then
	prerr_string "^"
      else
	prerr_string " "
    done;
    print_newline ();
    close_in ch


