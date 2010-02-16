open Base
open OUnit

let ok x y =
  assert_equal ~printer:Std.dump x y

let _ = begin "socket.ml" >::: [
  "send" >:: begin fun () ->
    let nc =
      Unix.open_process_in "nc -p 9000 -l" in
      Unix.sleep 1;
      Socket.connect_with  "127.0.0.1" 9000 ~f:begin fun sock ->
	Socket.send sock "hi\n"
      end;
      ok "hi\n" @@ Std.input_all nc
  end;
  "recv" >:: begin fun () ->
    let _ =
      Unix.open_process_out "echo hello | nc -p 9001 -l" in
      Unix.sleep 1;
      ok "hello" @@ Socket.connect_with  "127.0.0.1" 9001 ~f:begin fun sock ->
	Socket.recv sock 5
      end
  end;
] end +> run_test_tt_main
