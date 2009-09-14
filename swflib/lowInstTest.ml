open Base
open OUnit
open LowInst
open BytesOut

let ok x y =
  assert_equal x (to_bytes y)

let _ =
  ("lowInst.ml" >::: [
     "nop" >::
       (fun () ->
	  ok [u8 0x02] `Nop);
     "PushInt" >::
       (fun () ->
	  ok [u8 0x2d; u30 42] (`PushInt 42))

   ]) +> run_test_tt_main
