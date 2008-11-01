open Base

let assert_equal ?(msg="") =
  OUnit.assert_equal ~msg:msg ~printer:Std.dump

let assert_raises =
  OUnit.assert_raises

let compile_string str =
  ClosTrans.trans @@ Lisp.compile_string str
