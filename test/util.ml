let assert_equal ?(msg="") =
  OUnit.assert_equal ~msg:msg ~printer:Std.dump

let assert_raises =
  OUnit.assert_raises
