open Base
open Swflib
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
	  Template.make t (Abc.read (Stream.of_list bytes)) in
	  open_out_with t#output (fun ch -> Swf.write ch swf)
    | _ ->
	failwith "not suppert many files"
