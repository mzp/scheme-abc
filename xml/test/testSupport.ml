open Base
let ok x y =
  OUnit.assert_equal ~printer:Std.dump x y

let example name =
  let ch =
    open_in_bin @@ Printf.sprintf "../example/%s.abc" name in
    Abc.of_stream @@ Byte.of_channel ch


