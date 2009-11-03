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

let read path =
  if Filename.check_suffix path ".abc" then
    [open_in_with path (Abc.read $ Swflib.BytesIn.of_channel)]
  else if Filename.check_suffix path ".swf" then
    (open_in_with path Swf.read).tags
    +> filter_map (function `DoABC (_,_,abc) -> Some abc | _ -> None)
  else
    failwithf "unknow suffix: %s" path ()

let _ =
  let paths,t =
    CmdOpt.parse_argv () in
  let abc =
    HList.fold_left1 Link.link @@ HList.concat_map read paths in
  let swf =
    Template.make t abc in
    open_out_with t#output (fun ch -> Swf.write ch swf);
    open_out_with "__debug.abc" (fun ch -> List.iter (output_byte ch) @@ Abc.write abc)
