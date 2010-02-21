open Base
open Unix

type t =
    Client of in_channel * out_channel
  | Server of file_descr * file_descr

let connect host port =
  let inet =
    inet_addr_of_string host in
  let addr =
    ADDR_INET (inet, port) in
  let (ic, oc) =
    open_connection addr in
    Client (ic, oc)

let listen port =
  try
    let server =
      socket PF_INET SOCK_STREAM 0 in
    let _ =
      bind server (ADDR_INET (inet_addr_of_string "0.0.0.0", port));
      listen server 1 in
    let (client, _) =
      accept server in
      Server (client,server)
  with Unix_error (e,_,_) ->
    failwith @@ error_message e

let close = function
    Client (ic, _) ->
      shutdown_connection ic
  | Server (client, server) ->
      shutdown server SHUTDOWN_ALL;
      close server;
      shutdown client SHUTDOWN_ALL;
      close client

let listen_with ~f port =
  let sock =
    listen port in
    maybe f sock
    +> tee (fun _ -> close sock)
    +> function
	`Val v ->  v
      | `Error e -> raise e

let connect_with ~f host port =
  let sock =
    connect host port in
    maybe f sock
    +> tee (fun _ -> close sock)
    +> function
	`Val v ->  v
      | `Error e -> raise e

let out_channel = function
    Client (_, oc) -> oc
  | Server (fd, _) -> out_channel_of_descr fd

let in_descr = function
    Client (ic, _) -> descr_of_in_channel ic
  | Server (fd, _) -> fd

let send sock s =
  let oc =
    out_channel @@ sock in
    output_string oc s;
    flush oc

let recv sock len =
  let buffer =
    String.make len ' ' in
  let n =
    recv (in_descr sock) buffer 0 len [] in
    String.sub buffer 0 n
