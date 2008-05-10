open Base
open OUnit

let test1 = 
  TestCase (fun ()-> assert_equal [1;2;3] (range 1 4))

let tests =
  TestList [ test1 ]

let _ =
  run_test_tt_main tests
