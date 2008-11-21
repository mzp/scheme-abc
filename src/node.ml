open Base

type 'a t = {
  value:    'a;
  filename: string;
  lineno:   int;
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
    map (function
	     '\n' ->
	       {value   = '\n';
		filename= filename;
		lineno  = inc lineno}
	   | c ->
	       {value   = c;
		filename= filename;
		lineno  = !lineno})

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
  {value=a; filename="<empty>"; lineno=(-1)}

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

