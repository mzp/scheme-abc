open Base
open OUnit

let ok x y =
  assert_equal ~printer:Std.dump x y

let server_side () =
  Socket.listen_with 9000 ~f:begin fun t ->
    let s =
      Socket.recv t 3 in
      ok "foo" s;
      Socket.send t ("s:" ^ s)
  end

let client_side () =
  let s =
    Socket.connect_with "127.0.0.1" 9000 ~f:begin fun t ->
      Socket.send t "foo";
      Socket.recv t 5
    end in
    ok "s:foo" s

let _ = begin "socket.ml" >::: [
  "echo test" >:: begin fun () ->
    if Unix.fork () = 0 then begin
      server_side ()
    end else begin
      Unix.sleep 1;
      client_side ()
    end
  end ]
end +> run_test_tt_main

(*let _ = begin "socket.ml" >::: [
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
*)
