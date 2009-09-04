open Base
let _ =
  let argv =
    Array.to_list Sys.argv in
    match argv with
      _::xs ->
	xs +> List.iter (print_endline       $
			   Xml.to_string_fmt $
			   Swfmill.to_xml    $
			   Abc.of_stream     $
			   Byte.of_channel   $
			   open_in_bin)
      | [] ->
	  failwith "must not happen"
