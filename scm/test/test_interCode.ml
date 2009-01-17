open Base
open Util
open OUnit
open AstUtil
open InterCode

let v_ok variables program =
  let inter_code =
    of_program program in
    ok variables inter_code.variables

let m_ok methods program =
  let inter_code =
    of_program program in
    ok methods inter_code.methods

let _ =
  ("interCode.ml" >::: [
     "'define' should export its name" >::
       (fun () ->
	  v_ok [global "x"] [define (`Public (global "x")) (int 42)];
	  v_ok [] [define (`Internal (global "x")) (int 42)]);
     "'class' should export its name" >::
       (fun () ->
	  v_ok [global "Foo"]
	    [klass (`Public (global "Foo")) (global "Object") [] []];
	  v_ok []
	    [klass (`Internal (global "Foo")) (global "Object") [] []]);
     "'class' should export its member methods" >::
       (fun () ->
	  let k =
	    klass (`Public (global "Foo")) (global "Object") [] in
	    m_ok [sname "f"]
	      [k [meth "f" [] (int 42)]])
   ]) +> run_test_tt
