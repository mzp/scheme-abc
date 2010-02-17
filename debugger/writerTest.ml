open Base
open OUnit
open Writer

let _ = begin "writer.ml" >::: [
  "combinator" >:: begin fun () ->
    let data =
      db 42
      +>> db 10 in
      assert_equal "\042\010" @@ String.concat "" (data [])
  end
] end +> run_test_tt_main


