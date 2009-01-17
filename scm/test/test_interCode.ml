open Base
open Util
open OUnit
open AstUtil
open InterCode

let v_ok variables program =
  let inter_code =
    of_program program in
    ok variables inter_code.variables


let _ =
  ("interCode.ml" >::: [
     "'public define' should export its name" >::
       (fun () ->
	  v_ok [global "x"] [define (`Public (global "x")) (int 42)];
	  v_ok [] [define (`Internal (global "x")) (int 42)]);
   ]) +> run_test_tt
