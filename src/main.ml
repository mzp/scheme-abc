open Base

let generate path stream =
  let ast =
    Lisp.compile stream in
  let abc = 
    Ast.generate ast in
  let bytes =
    Abc.bytes_of_abc abc in
  let ch = 
    open_out_bin path in
    Bytes.output_bytes ch bytes;
    close_out ch

let main () =
  let input =
    Sys.argv.(1) in
  let ch =
    open_in input in
  let output =
    "a.abc" in
    generate output @@ Stream.of_channel ch

let _ =
  if not !Sys.interactive then begin
    main ()
  end
  
