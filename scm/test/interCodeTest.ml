open Base
open Util
open OUnit
open AstUtil
open InterCode

let v_ok variables program =
  let tbl =
    add empty "Foo" @@ of_program program in
    ok true @@
      List.for_all (flip InterCode.mem_variable tbl) variables;
    ok false @@
      List.for_all (flip InterCode.mem_variable tbl) ["Bar","z"]

let m_ok methods program =
  let tbl =
    add empty "Foo" @@ of_program program in
    ok true @@
      List.for_all (flip InterCode.mem_method tbl) methods;
    ok false @@
      List.for_all (flip InterCode.mem_method tbl) ["A";"B"]

let _ =
  ("interCode.ml" >::: [
     "'define' should export its name" >::
       (fun () ->
	  v_ok ["Foo","x"] [define (`Public (global "x")) (int 42)];
	  v_ok [] [define (`Internal (global "x")) (int 42)]);
     "'class' should export its name" >::
       (fun () ->
	  v_ok ["Foo","Bar"]
	    [klass (`Public (global "Bar")) (global "Object") [] []];
	  v_ok []
	    [klass (`Internal (global "Foo")) (global "Object") [] []]);
     "'class' should export its member methods" >::
       (fun () ->
	  let k m =
	    klass (`Public (global "Bar")) (global "Object") [] m in
	    m_ok ["f"]
	      [k [meth "f" [] (int 42)]]);
   ]) +> run_test_tt
