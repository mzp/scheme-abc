open Base
open OUnit
open HighInst

let _ =
  ("highInst.ml" >::: [
     "const" >::
       (fun () ->
	  assert_equal [`Int 42] (HighInst.const (`PushInt 42));
	  assert_equal [`String "foo"] (HighInst.const (`PushString "foo")))
   ]) +> run_test_tt_main
