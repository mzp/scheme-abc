open Base
open Swflib.SwfType

let input_bytes ch =
  let xs =
    ref [] in
    try
      while true do
	xs := (input_byte ch)::!xs
      done;
      failwith "must not happen"
    with _ ->
      List.rev !xs

let _ =
  match CmdOpt.parse_argv () with
      [path], t ->
	let bytes =
	  open_in_with path input_bytes in
	let swf =
	  Template.make t bytes in
	  swf
    | _ ->
	failwith "not suppert many files"
