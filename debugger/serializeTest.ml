open Base
open OUnit

open Serialize.M
open Serialize

let ok x y =
  assert_equal x @@ snd @@ runWriter y

let _ = begin "serialize.ml" >::: [
  "monad" >:: begin fun () ->
    let m = perform with module Serialize.M in begin
	db 0;
	db 1
    end in
    ok "\000\001" m
   end;
  "db" >:: begin fun () ->
    ok "\042" @@ db 42;
    assert_raises Out_of_range begin fun () ->
      db 0x1FF
    end
  end;
  "dw" >:: begin fun () ->
    ok "\x01\x02" @@ dw 0x0201;
    assert_raises Out_of_range begin fun () ->
      db 0x1_0000
    end
  end;
  "dd" >:: begin fun () ->
    ()
  end;
  "sz" >:: begin fun () ->
    ()
  end
] end +> run_test_tt_main
