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

let abc_of_swf _ = undef

let read path =
  if Filename.check_suffix path ".abc" then
    open_in_with path (Abc.read $ Swflib.BytesIn.of_channel)
  else if Filename.check_suffix path ".swf" then
    abc_of_swf @@ open_in_with path Swf.read
  else
    failwithf "unknow suffix: %s" path ()

let _ =
  match CmdOpt.parse_argv () with
      [path], t ->
	let s =
	  open_in_with path (Abc.read $ Swflib.BytesIn.of_channel) in
	let swf =
	  Template.make t s in
	  open_out_with t#output (fun ch -> Swf.write ch swf)
    | _ ->
	failwith "not suppert many files"
