open Base
open OUnit

module ListMonoid = struct
  type t = string list
  let mempty = []
  let mappend = (@)
end

module StrMonoid = struct
  type t = string
  let mempty = ""
  let mappend = (^)
end

module W = Writer.Make(ListMonoid)
module W2 = Writer.Make(StrMonoid)
open W

let _ = begin "writer.ml" >::: [
  "tell" >:: begin fun () ->
    let m =
      perform with module W in begin
        tell ["hi"];
	tell ["ho"];
	ret ()
      end in
    assert_equal () @@ fst @@ runWriter m;
    assert_equal ["hi"; "ho"] @@ snd @@ runWriter m
  end;
  "with_str" >:: begin fun () ->
    open W2 in
    let m =
      perform with module W2 in begin
        tell "hi";
	tell "ho";
	ret ()
      end in
    assert_equal () @@ fst @@ runWriter m;
    assert_equal "hiho" @@ snd @@ runWriter m
  end
] end +> run_test_tt_main
