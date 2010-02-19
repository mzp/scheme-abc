open Base
open OUnit

open Serialize.M
open Serialize

let _ = begin "serialize.ml" >::: [
  "monad" >:: begin fun () ->
    let m = perform with module Serialize.M in begin
	db 42
    end in
    let data =
      snd @@ runWriter m in
      assert_equal "" data
  end
] end +> run_test_tt_main
