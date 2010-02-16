open Base

type t = in_channel * out_channel

let connect host port =
  let inet =
    Unix.inet_addr_of_string host in
  let addr =
    Unix.ADDR_INET (inet, port) in
    Unix.open_connection addr

let close (ic, _) =
  Unix.shutdown_connection ic

let send (_, oc) s =
  output_string oc s

let recv (ic, _) len =
  let buffer =
    String.make len ' ' in
  let n =
    Unix.recv (Unix.descr_of_in_channel ic) buffer 0 len [] in
    String.sub buffer 0 n
