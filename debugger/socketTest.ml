open Base
open OUnit

(* stub server *)
let server port f =
  if Unix.fork () = 0 then
    let sa =
      Unix.ADDR_INET (Unix.inet_addr_any, port) in
      Unix.establish_server f sa

let last_msg = ref ""

let send_test port f =
  server port begin fun ic _ ->
    try f () with _ -> ();
    last_msg := input_line ic;
    Unix.shutdown_connection ic
  end

let recv_test port f =
  server port begin fun ic oc ->
    output_string oc "hi!";
    try f () with _ -> ();
    Unix.shutdown_connection ic
  end

let _ = begin "socket.ml" >::: [
  "send" >:: begin fun () ->
    send_test 9000 begin fun () ->
      let socket =
	Socket.connect "localhost" 9000 in
	Socket.send socket "hi";
	assert_equal "hi" !last_msg;
	Socket.close socket
    end;
  end;
  "recv" >:: begin fun () ->
    recv_test 9001 begin fun () ->
      let socket =
	Socket.connect "localhost" 9001 in
	assert_equal "hi!" @@ Socket.recv socket 3;
	Socket.close socket
    end;
  end;
] end +> run_test_tt_main
