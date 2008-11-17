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
  
let with_line filename stream =
  let lineno =
    ref 0 in
    Stream.from 
      (fun _ ->
	 try
	   match Stream.next stream with
	       '\n' ->
		 Some {value   = '\n';
		       filename= filename;
		       lineno  = inc lineno}
	     | c ->
		 Some {value   = c;
		       filename= filename;
		       lineno  = !lineno}
	 with Stream.Failure ->
	   None)

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

